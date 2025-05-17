#if 0
gcc -s -O2 -o ~/bin/zorkmid -Wno-unused-result zorkmid.c #
exit #
#endif
/*
  ZORKMID -- Zork Machine Interpreter and Debugger
  This program is in the public domain.
*/

#define ZVERSION "" // Z-code version; "" or "E" or "X" or "Y"
#define IVERSION 9 // program version

#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <time.h>

typedef unsigned char byte;
typedef uint16_t uzint;
typedef int16_t zint;
typedef uint32_t uint32;
typedef int32_t sint32;

static byte option_set[128];
static char*option_data[128];

static FILE*story;
static int is_tandy;
static int screen_rows;
static int screen_columns;
static int restrict_saving;
static int embrace_fwords;
static int warnings;
static int debug_on_quit;
static int halt_on_error;
static int tracing_execution;
static int tracing_zchars;
static FILE*script;
static FILE*savegame;
static char escape;
static FILE*randstream;

typedef struct {
  int dstackptr;
  sint32 pc;
  int numlocals;
  zint locals[16];
} Frame;

static uzint endlod;
static uzint vocab;
static uzint object;
static uzint global;
static uzint purbot;
static uzint fwords;
static uint32 game_size;

static byte mem[0x20000];
static sint32 pc;
static sint32 instpc;
static int dstackptr;
static int dstack_highwater=0;
static int cstackptr;
static int cstack_highwater=0;
static int totalstack_highwater=0;
static zint dstack[512];
static Frame cstack[256];
static char textbuf[256];
static int textptr;
static int curpos;
static int lines; // number of lines before displaying [MORE] prompt
static zint args[4];
static byte argn;
static char sib[128];
static int doing_usl;

static jmp_buf exception_buffer;
static int pc_changed;
static sint32 debugptr;
static byte instbreak[256];
static sint32 pcbreak[32];
static int in_debugger;
static int inststep;
static int counter;
static int profile[256];

typedef struct {
  int file;
  int line;
  int column;
  sint32 pc;
} DebugLineRec;

typedef struct {
  char*name;
  zint code;
   // $001-$00F=local $010-$0FF=global $100-$11F=flag $180-$19F=property
   // $200=object $201=address $202=misc $203=function $204=text $205=table
   // $206=byte-table $207=word-table
  sint32 begin;
  sint32 end;
} DebugSymbolRec;

static int debug_file_count;
static char**debug_file;
static int debug_line_count;
static DebugLineRec*debug_line;
static int debug_symbol_count;
static DebugSymbolRec*debug_symbol;

typedef struct {
  const char*text;
  byte flag; // 1=value 2=predicate 4=inline-text 8=variable-number
} Mnemonic;

static const Mnemonic mnemonic[256]={
  [1]={"EQUAL?",2},
  [2]={"LESS?",2},
  [3]={"GRTR?",2},
  [4]={"DLESS?",10},
  [5]={"IGRTR?",10},
  [6]={"IN?",2},
  [7]={"BTST",2},
  [8]={"BOR",1},
  [9]={"BAND",1},
  [10]={"FSET?",2},
  [11]={"FSET",0},
  [12]={"FCLEAR",0},
  [13]={"SET",8},
  [14]={"MOVE",0},
  [15]={"GET",1},
  [16]={"GETB",1},
  [17]={"GETP",1},
  [18]={"GETPT",1},
  [19]={"NEXTP",1},
  [20]={"ADD",1},
  [21]={"SUB",1},
  [22]={"MUL",1},
  [23]={"DIV",1},
  [24]={"MOD",1},
  [128]={"ZERO?",2},
  [129]={"NEXT?",3},
  [130]={"FIRST?",3},
  [131]={"LOC",1},
  [132]={"PTSIZE",1},
  [133]={"INC",8},
  [134]={"DEC",8},
  [135]={"PRINTB",0},
  [137]={"REMOVE",0},
  [138]={"PRINTD",0},
  [139]={"RETURN",0},
  [140]={"JUMP",0},
  [141]={"PRINT",0},
  [142]={"VALUE",9},
  [143]={"BCOM",1},
  [176]={"RTRUE",0},
  [177]={"RFALSE",0},
  [178]={"PRINTI",4},
  [179]={"PRINTR",4},
  [180]={"NOOP",0},
  [181]={"SAVE",2},
  [182]={"RESTORE",2},
  [183]={"RESTART",0},
  [184]={"RSTACK",0},
  [185]={"FSTACK",0},
  [186]={"QUIT",0},
  [187]={"CRLF",0},
  [188]={"USL",0},
  [189]={"VERIFY",2},
  [224]={"CALL",1},
  [225]={"PUT",0},
  [226]={"PUTB",0},
  [227]={"PUTP",0},
  [228]={"READ",0},
  [229]={"PRINTC",0},
  [230]={"PRINTN",0},
  [231]={"RANDOM",1},
  [232]={"PUSH",0},
  [233]={"POP",8},
  [234]={"SPLIT",0},
  [235]={"SCREEN",0},
};

static const char cs2ascii[32]=" \e\e\e\e\e\e\r0123456789.,!?_#'\"/\\-:()";

#define get16(a) ((zint)((mem[(a)+(mem[1]&1)]<<8)|mem[(a)+!(mem[1]&1)]))
#define put16(a,b) (mem[(a)+(mem[1]&1)]=(b)>>8,mem[(a)+!(mem[1]&1)]=(b)&255)
#define get8(a) (mem[(a)])
#define put8(a,b) (mem[(a)]=(b))
#define hdrflags() (mem[17-(mem[1]&1)])

static void fatal(const char*s) {
  fprintf(stderr,"%s\n",s);
  exit(1);
}

static void warn(const char*s) {
  if(!warnings) return;
  fprintf(stderr,"<%s>",s);
}

static inline void begin_story_text(void) {
}

static inline void begin_system_text(void) {
}

static inline void reread_header_flags(void) {
}

static void restart(int midgame);

static void linebreak(void);

static sint32 zprint(sint32 ad);

static void dumptext(void);

static void unassemble(void) {
  static byte m[4]={0x5F,0x6F,0x9F,0xAF};
  sint32 p=debugptr;
  sint32 q,r;
  int x,y,z;
  if(p<0 || p>=0x20000) return;
  printf("%5u: ",p);
  x=mem[p++];
  if(x<128) {
    y=m[(x>>5)&3];
    x&=31;
  } else if(x<176) {
    y=(x<<2)|63;
    x&=143;
  } else if(x<192) {
    y=255;
  } else {
    y=mem[p++];
    if(x<224) x&=31;
  }
  if(!mnemonic[x].text) {
    printf("%02X __ __ __ __ __ __ __ __ __ __ __  <Unknown>\n",mem[debugptr++]);
    return;
  }
  p+=__builtin_popcount(255&~y);
  if(mnemonic[x].flag&1) p++;
  if(mnemonic[x].flag&2) {
    z=mem[p++];
    if(!(z&64)) p++;
  }
  if(p<0 || p>=0x20000) {
    puts("<End of memory>");
    return;
  }
  for(q=debugptr;q<p;q++) printf("%02X ",mem[q]);
  for(;q<debugptr+12;q++) printf("__ ");
  q=p;
  p=debugptr+1+(mem[debugptr]>=192);
  printf(" %s",mnemonic[x].text);
  for(z=0;z<4;z++) {
    switch((y>>6)&3) {
      case 0:
        printf(" %d",get16(p));
        if(x==140) printf(" (%d)",get16(p)+p);
        p+=2;
        break;
      case 1:
        printf(" %u",mem[p]);
        if(x==140) printf(" (%d)",get16(p)+p+1);
        p++;
        break;
      case 2:
        if(mem[p]) printf(" VAR(%u)",mem[p]); else printf(" STACK");
        p++;
        break;
    }
    y<<=2;
  }
  if(mnemonic[x].flag&1) {
    if(mem[p]) printf(" >VAR(%u)",mem[p]); else printf(" >STACK");
    p++;
  }
  if(mnemonic[x].flag&2) {
    printf(" %c",mem[p]&128?'/':'\\');
    if(mem[p]&64) r=(z=mem[p]&63)+p-1; else r=(z=((mem[p]&63)<<8)|mem[p+1])+p;
    if(z&0x2000) r-=0x4000;
    if(z==0) printf("RFALSE"); else if(z==1) printf("RTRUE"); else if(z!=2) printf("%u",r);
  } else if(mnemonic[x].flag&4) {
    while(q<0x1FFFF && !(get16(q)&0x8000)) q+=2;
    q+=2;
  }
  putchar(10);
  debugptr=q;
}

static void call_return(zint r);

static uzint vocab_lookup(char*buf,int len);

static int add_debugger_symbols(char*buf) {
  int w,x,y,z;
  switch(*buf) {
    case 'F':
      x=debug_file_count++;
      debug_file=realloc(debug_file,sizeof(char*)*debug_file_count);
      debug_file[x]=malloc(strlen(buf));
      sscanf(buf,"F <%[^>]>",debug_file[x]);
      return 1;
    case 'L':
      x=debug_line_count++;
      debug_line=realloc(debug_line,sizeof(DebugLineRec)*debug_line_count);
      sscanf(buf,"L %i %i %i %i",&debug_line[x].file,&debug_line[x].line,&debug_line[x].column,&y);
      debug_line[x].pc=y;
      return 1;
    case 'X':
      x=debug_symbol_count++;
      debug_symbol=realloc(debug_symbol,sizeof(DebugSymbolRec)*debug_symbol_count);
      debug_symbol[x].name=malloc(strlen(buf));
      sscanf(buf,"X %i <%[^>]> %i %i",&w,debug_symbol[x].name,&y,&z);
      debug_symbol[x].code=w;
      debug_symbol[x].begin=y;
      debug_symbol[x].end=z;
      return 1;
    default:
      return 0;
  }
}

static sint32 find_debug_line(int fn,int ln) {
  int i;
  for(i=0;i<debug_line_count;i++) if(debug_line[i].file==fn && debug_line[i].line==ln) return debug_line[i].pc;
  return 0;
}

static int debugger_exec(char*buf) {
  int x,y;
  while(*buf==32) buf++;
  switch(*buf) {
    case 'a':
      pc=instpc;
      pc_changed=1;
      return 1;
      break;
    case 'b':
      for(x=0;x<32;x++) {
        if(!pcbreak[x] || pcbreak[x]==strtol(buf+1,0,0)) {
          pcbreak[x]=buf[1]==':'?find_debug_line(0,strtol(buf+1,0,0)):strtol(buf+1,0,0);
          if(pcbreak[x]) printf("Set breakpoint [%d] at %d.\n",x,pcbreak[x]);
          else puts("Invalid breakpoint.");
          break;
        }
      }
      if(x==32) puts("Breakpoint list is full.");
      break;
    case 'c':
      return 1;
      break;
    case 'd':
      if(buf[1]>32) debugptr=strtol(buf+1,0,0);
      for(x=0;x<8;x++) {
        printf("[%05X] ",debugptr);
        for(y=0;y<16;y++) if(debugptr<0x20000 && debugptr>=0) printf("%02X ",mem[debugptr++]);
        putchar(10);
      }
      break;
    case 'e':
      buf++;
      x=strtol(buf,&buf,0);
      y=strtol(buf,&buf,0);
      puts("OK.");
      mem[x]=y;
      break;
    case 'g':
      pc=strtol(buf+1,0,0);
      pc_changed=1;
      puts("OK.");
      break;
    case 'i':
      x=strtol(buf+1,0,0)&255;
      if(mnemonic[x].text) {
        if(instbreak[x]) {
          printf("Cleared instruction breakpoint %d (%s).\n",x,mnemonic[x].text);
          instbreak[x]=0;
          if(x<128) instbreak[x+32]=instbreak[x+64]=instbreak[x+96]=instbreak[x+192]=0;
          else if(x<176) instbreak[x+16]=instbreak[x+32]=0;
        } else {
          printf("Set instruction breakpoint %d (%s).\n",x,mnemonic[x].text);
          instbreak[x]=1;
          if(x<128) instbreak[x+32]=instbreak[x+64]=instbreak[x+96]=instbreak[x+192]=1;
          else if(x<176) instbreak[x+16]=instbreak[x+32]=1;
        }
      } else {
        puts("Unknown opcode.");
      }
      break;
    case 'k':
      pc=instpc;
      x=mem[pc++];
      if(x<128) {
        pc+=2;
        x&=31;
      } else if(x<176) {
        pc+=1+(x<159);
        x&=143;
      } else if(x>=192) {
        pc+=__builtin_popcount(255&~mem[pc])+1;
        if(x<224) x&=31;
      }
      if(mnemonic[x].flag&1) pc++;
      if(mnemonic[x].flag&2) {
        y=mem[pc++];
        if(!(y&64)) pc++;
      }
      if(mnemonic[x].flag&4) while(pc<0x1FFFF && !(get16(pc)&0x8000)) pc+=2;
      pc_changed=1;
      return 1;
      break;
    case 'o':
      x=strtol(buf+1,0,0);
      printf("HDR=%d FLAGS=",object+53+x*9);
      for(y=0;y<32;y++) putchar((mem[object+53+x*9+((y>>3)^(mem[1]&1))]<<(y&7))&128?'1':'0');
      printf(" LOC=%d NEXT=%d FIRST=%d PT=%d\n",mem[object+57+x*9],mem[object+58+x*9],mem[object+59+x*9],debugptr=get16(object+60+x*9));
      debugptr+=(mem[debugptr]<<1)+1;
      while(mem[debugptr]) {
        printf("[%d]",mem[debugptr]&31);
        for(y=(mem[debugptr]>>5)+1;y;y--) printf(" %02X",mem[++debugptr]);
        ++debugptr;
        putchar(10);
      }
      break;
    case 'p':
      zprint(strtol(buf+1,0,0));
      putchar(10);
      break;
    case 'q':
      exit(1);
      break;
    case 'r':
      restart(1);
      pc_changed=1;
      break;
    case 's':
      inststep=strtol(buf+1,0,0);
      if(inststep<=0) inststep=1;
      return 1;
      break;
    case 'u':
      if(buf[1]>32) debugptr=strtol(buf+1,0,0);
      for(x=0;x<8;x++) unassemble();
      break;
    case 'v':
      x=strtol(buf+1,0,0);
      if(x<0 || x>31) {
        puts("Breakpoint number out of range.");
        break;
      }
      if(pcbreak[x]) {
        printf("Cleared breakpoint [%d] at %d.\n",x,pcbreak[x]);
        pcbreak[x]=0;
      } else {
        puts("Breakpoint not set.");
      }
      break;
    case 'x':
      x=strtol(buf+1,0,0);
      printf("%s = %u, %d, 0x%X\n",buf+1,(uzint)x,(zint)x,x);
      break;
    case 'z':
      for(x=0;x<256;x++) instbreak[x]=0;
      for(x=0;x<32;x++) pcbreak[x]=0;
      puts("OK.");
      break;
    case '=':
      printf("Ptr = %d (0x%X)\n",debugptr=instpc,instpc);
      break;
    case '.':
      printf("CS=%d DS=%d PC=%d\nLines=%d Curpos=%d Textptr=%d\n",cstackptr,dstackptr,pc,lines,curpos,textptr);
      for(x=0;x<argn;x++) printf("Args[%d]=%d\n",x,args[x]);
      printf("Highwater: CS=%d DS=%d TOTAL=%d\n",cstack_highwater,dstack_highwater,totalstack_highwater);
      break;
    case '\\':
      dumptext();
      begin_system_text();
      putchar(10);
      break;
    case '+':
      buf++;
      x=strtol(buf,&buf,0);
      y=strtol(buf,&buf,0);
      printf("%d (0x%X)\n",x+y,x+y);
      break;
    case '-':
      buf++;
      x=strtol(buf,&buf,0);
      y=strtol(buf,&buf,0);
      printf("%d (0x%X)\n",x-y,x-y);
      break;
    case '*':
      buf++;
      x=strtol(buf,&buf,0);
      y=strtol(buf,&buf,0);
      printf("%d (0x%X)\n",x*y,x*y);
      break;
    case '/':
      buf++;
      x=strtol(buf,&buf,0);
      y=strtol(buf,&buf,0);
      if(y) printf("%d (0x%X)\n",x/y,x/y);
      break;
    case '%':
      buf++;
      x=strtol(buf,&buf,0);
      y=strtol(buf,&buf,0);
      if(y) printf("%d (0x%X)\n",x%y,x%y);
      break;
    case '\'':
      call_return(strtol(buf+1,0,0));
      pc_changed=1;
      puts("OK.");
      break;
    case '[':
      if(!dstackptr) puts("Empty stack.");
      else printf("%d\n",dstack[--dstackptr]);
      break;
    case ']':
      dstack[dstackptr++]=strtol(buf+1,0,0);
      puts("OK.");
      break;
    case 'B':
      for(x=0;x<32;x++) if(pcbreak[x]) printf("[%d] %d\n",x,pcbreak[x]);
      break;
    case 'C':
      for(x=0;x<cstackptr;x++) {
        printf("[%d] PC=%d SP=%d Locals=%d\n",x,cstack[x].pc,cstack[x].dstackptr,cstack[x].numlocals);
        for(y=1;y<=cstack[x].numlocals;y++) printf("  VAR(%d) = %d\n",y,cstack[x].locals[y]);
      }
      break;
    case 'D':
      for(x=0;x<dstackptr;x++) printf("%3d: %d\n",x,dstack[x]);
      break;
    case 'H':
      printf("ENDLOD=%d PURBOT=%d VOCAB=%d OBJECT=%d GLOBAL=%d FWORDS=%d\n",endlod,purbot,vocab,object,global,fwords);
      break;
    case 'I':
      for(x=0;x<256;x++) if(instbreak[x] && mnemonic[x].text) printf("[%d] %s\n",x,mnemonic[x].text);
      break;
    case 'O':
      for(x=0;x<256;x++) profile[x]=0;
      puts("OK.");
      break;
    case 'P':
      if(buf[1]>32) {
        printf("%d\n",profile[strtol(buf+1,0,0)]);
      } else {
        for(x=0;x<256;x++) {
          if(x==0x00) puts("2OP(imm,imm):");
          if(x==0x20) puts("2OP(imm,var):");
          if(x==0x40) puts("2OP(var,imm):");
          if(x==0x60) puts("2OP(var,var):");
          if(x==0x80) puts("1OP(long):");
          if(x==0x90) puts("1OP(short):");
          if(x==0xA0) puts("1OP(var):");
          if(x==0xB0) puts("0OP:");
          if(x==0xC0) puts("EXT:");
          if(profile[x]) {
            printf("  %3d: %8d",x,profile[x]);
            y=x&(x<128?31:x<176?143:x<192?255:x<224?31:255);
            if(mnemonic[y].text) printf(" [%s]",mnemonic[y].text);
            putchar(10);
          }
        }
      }
      break;
    case 'V':
      for(x=0;buf[x]>32;x++);
      printf("%d\n",vocab_lookup(buf+1,x-1));
      break;
    case 'W':
      dstack_highwater=cstack_highwater=totalstack_highwater=0;
      puts("OK.");
      break;
    case '|':
      puts(add_debugger_symbols(buf+1)?"OK.":"Unknown command.");
      break;
    default:
      if(*buf!=';' && *buf>32) puts("Unknown command.");
  }
  return 0;
}

static void debugger_show_line(void) {
  int i;
  for(i=0;i<debug_line_count;i++) {
    if(debug_line[i].pc==instpc) {
      printf("@ %s:%d\n",debug_file[debug_line[i].file],debug_line[i].line);
      return;
    }
  }
}

static void debugger(const char*msg) {
  char buf[256];
  inststep=0;
  begin_system_text();
  printf("\n<! %s>\nPC: 0x%05X (0x%05X)\n",msg,pc,instpc);
  if(debug_line) debugger_show_line();
  if(halt_on_error || in_debugger) fatal("Program halted.");
  debugptr=instpc;
  in_debugger=1;
  for(;;) {
    putchar('*');
    if(!fgets(buf,256,stdin)) fatal("Unable to read standard input.");
    if(debugger_exec(buf)) break;
  }
  lines=screen_rows-1;
  in_debugger=0;
  if(pc_changed) longjmp(exception_buffer,1);
}

static void exec_escape(char*buf) {
  sint32 a;
  begin_system_text();
  switch(*buf) {
    case '1':
      lines=1;
      break;
    case '=':
      lines=-1;
      break;
    case 'c':
      printf("%d\n",counter);
      break;
    case 'd':
      debugger("User requested debugger.");
      break;
    case 'q':
      exit(0);
      break;
    case 'r':
      restart(1);
      puts("\n\n\n\n\n\n\n\n");
      longjmp(exception_buffer,1);
      break;
    case 's':
      if(script) fclose(script);
      script=0;
      if(buf[1]) script=fopen(buf+1,"w");
      break;
    case 't':
      dumptext();
      linebreak();
      a=get16(object+60+get16(global)*9);
      zprint(a+1);
      dumptext();
      linebreak();
      begin_system_text();
      if(mem[1]&2) {
        printf("Time: %02d:%02d\n",get16(global+2),get16(global+4));
      } else {
        printf("Score: %d    Moves: %d\n",get16(global+2),get16(global+4));
      }
      break;
    case 'v':
      begin_system_text();
      printf("%sZORKMID version %d; implements %sZIP.\n",ZVERSION,IVERSION,ZVERSION);
      break;
    case ';':
      if(script) fprintf(script,"\n:: %s\n>",buf+1);
      break;
    default:
      printf("<Unknown escape command.>\n");
  }
  begin_story_text();
}

static void linebreak(void) {
  char buf[256];
  begin_story_text();
  putchar(10);
  if(script && (hdrflags()&1)) fputc(10,script);
  curpos=0;
  if(screen_rows!=-1 && !--lines) {
    begin_system_text();
    more_again:
    printf("<MORE>");
    fgets(buf,256,stdin);
    if(*buf=='1') {
      lines=1;
    } else if(*buf=='=') {
      lines=-1;
    } else if(escape && *buf==escape) {
      exec_escape(buf+1);
      goto more_again;
    } else {
      lines=screen_rows-1;
    }
  }
}

static void dumptext(void) {
  int i;
  begin_story_text();
  if(curpos+textptr>screen_columns-1) linebreak();
  for(i=0;i<textptr;i++) {
    putchar(textbuf[i]);
    if(script && (hdrflags()&1)) fputc(textbuf[i],script);
  }
  curpos+=textptr;
  textptr=0;
}

static void zprintchar(byte ch) {
  if(tracing_zchars) printf("[%d]",ch);
  if(!ch) return;
  if(in_debugger) {
    if(ch==13) putchar(10); else putchar(ch);
  } else if(doing_usl && doing_usl<screen_columns) {
    if(ch==13) doing_usl=screen_columns; else putchar(ch);
    doing_usl++;
  } else if(screen_columns==-1) {
    begin_story_text();
    if(ch==13) putchar(10);
    else if(ch) putchar(ch);
    if(script && (hdrflags()&1)) fputc(ch,script);
  } else if(ch==13) {
    dumptext();
    linebreak();
  } else if(ch==32 || ch==9) {
    if(textptr) {
      dumptext();
      if(curpos) {
        putchar(32);
        curpos++;
      }
    } else {
      begin_story_text();
      putchar(32);
      curpos++;
    }
  } else if((ch&128) || (ch>0 && ch<32)) {
    debugger("Character code out of range.");
  } else if(ch) {
    if(textptr>=screen_columns || textptr>250) return;
    textbuf[textptr++]=ch;
  }
}

static sint32 zprint(sint32 ad) {
  static int in_fword=0;
  uzint v=0;
  int i,j;
  int ts=0;
  int ps=0;
  while(!(v&0x8000)) {
    while(ad>=game_size) debugger("Text past end of file.");
    v=get16(ad);
    ad+=2;
    for(i=0;i<3;i++) {
      j=(v>>(10-5*i))&31;
      if(tracing_zchars) printf("<%d.%d.%d>",j,ts,ps);
      if(ts<3) {
        switch(j) {
          case 0:
            if(ts!=ps) warn("Temporary and permanent charsets do not match.");
            zprintchar(32);
            break;
          case 1 ... 3:
            if(ts!=ps) warn("Temporary and permanent charsets do not match.");
            ts=j+2;
            break;
          case 4:
            if(ts) ts=ps=ts&1; else ts=1;
            break;
          case 5:
            if(ts) ts=ps=ts&2; else ts=2;
            break;
          default:
            if(j==6 && ts==2) {
              ts=6;
              break;
            } else if(ts==2) {
              zprintchar(cs2ascii[j]);
            } else if(ts==1) {
              zprintchar(j+'A'-6);
            } else if(ts==0) {
              zprintchar(j+'a'-6);
            }
            ts=ps;
            break;
        }
      } else if(ts<6) {
        if(fwords+(j+(ts-3)*32)*2+1>=endlod) debugger("Frequent word header beyond ENDLOD.");
        if(in_fword) debugger("Fword inside of another fword.");
        in_fword=1;
        if(embrace_fwords) zprintchar('{');
        zprint(((sint32)get16(fwords+(j+(ts-3)*32)*2))<<1);
        if(embrace_fwords) zprintchar('}');
        in_fword=0;
        ts=ps;
      } else if(ts==6) {
        ts=(j<<5)|256;
        if(j>3) warn("ASCII escape out of range.");
      } else {
        zprintchar((ts|j)&255);
        ts=ps;
      }
    }
  }
  return ad;
}

static inline void zprintnum(zint n) {
  char buf[8];
  char*p=buf;
  sprintf(buf,"%d",n);
  while(*p) zprintchar(*p++);
}

static byte randbyte(void) {
  sint32 x;
  static sint32 mt[624];
  static int ind=0;
  static int mtinit=1;
  if(randstream) {
    x=fgetc(randstream);
    while(x==EOF) debugger("Random number stream has ended.");
    return x;
  }
  if(mtinit) {
    *mt=time(0)+31337;
    for(x=1;x<624;x++) mt[x]=1812433253L*(mt[x-1]^(mt[x-1]>>30))+x;
    mtinit=0;
  }
  if(!ind) {
    mt[0]-=time(0)/31;
    mt[1]+=clock()/5;
    for(ind=0;ind<624;ind++) {
      x=(mt[ind]&0x80000000L)|(mt[(ind+1)%624]&0x7FFFFFFFL);
      mt[ind]=mt[(ind+397)%624]^(x>>1);
      if(x&1) mt[ind]^=2567483615UL;
    }
    ind=0;
  }
  ind=(ind+1)%624;
  x=mt[ind];
  x^=x>>11;
  x^=(x<<7)&2636928640UL;
  x^=(x<<15)&4022730752UL;
  x^=x>>18;
  if(ind&1) x^=clock()/17;
  return x&255;
}

static zint randgen(zint max) {
  zint x;
  zint m;
  if(max==1) return 1;
  if(max<=0) debugger("Random number too small.");
  m=max-1;
  m|=m>>1;
  m|=m>>2;
  m|=m>>4;
  m|=m>>8;
  for(;;) {
    x=randbyte();
    if(m&~255) {
      x=((x<<8)|randbyte())&m;
    } else {
      x&=m;
    }
    if(++x>max) continue;
    return x;
  }
}

static inline void update_highwater(void) {
  if(dstackptr>dstack_highwater || cstackptr>cstack_highwater) {
    int i;
    int t=dstackptr;
    if(dstackptr>dstack_highwater) dstack_highwater=dstackptr;
    if(cstackptr>cstack_highwater) cstack_highwater=cstackptr;
    for(i=t=0;i<cstackptr;i++) t+=cstack[i].numlocals+4;
    if(t>totalstack_highwater) totalstack_highwater=t;
  }
}

static inline void push(zint v) {
  while(dstackptr==512) debugger("Data stack overflow.");
  dstack[dstackptr++]=v;
  update_highwater();
}

static inline zint pop(void) {
  while(dstackptr==(cstackptr?cstack[cstackptr-1].dstackptr:0)) debugger("Data stack underflow.");
  return dstack[--dstackptr];
}

static inline zint fetch(byte v) {
  if(v>15) {
    while((v<<1)+global-31>=endlod) debugger("Access to global variable beyond ENDLOD.");
    return get16((v<<1)+global-32);
  } else if(v) {
    while(!cstackptr || v>cstack[cstackptr-1].numlocals) debugger("Access to undefined local variable.");
    return cstack[cstackptr-1].locals[v];
  } else {
    return pop();
  }
}

#define pc_limit(a) do { if(pc+(a)>=game_size || pc+(a)<64) fatal("PC past end of file."); } while(0)
#define operand_limit(a,b) do { while(argn<a || argn>b) debugger("Wrong number of operands."); } while(0)

static void store(zint s) {
  byte v;
  pc_limit(1);
  v=mem[pc++];
  if(v>15) {
    while((v<<1)+global-31>=purbot) debugger("Access to global variable beyond PURBOT.");
    put16((v<<1)+global-32,s);
  } else if(v) {
    while(!cstackptr || v>cstack[cstackptr-1].numlocals) debugger("Access to undefined local variable.");
    cstack[cstackptr-1].locals[v]=s;
  } else {
    push(s);
  }
}

static inline zint vfetch(byte v) {
  if(v>15) {
    while((v<<1)+global-31>=endlod) debugger("Access to global variable beyond ENDLOD.");
    return get16((v<<1)+global-32);
  } else if(v) {
    while(!cstackptr || v>cstack[cstackptr-1].numlocals) debugger("Access to undefined local variable.");
    return cstack[cstackptr-1].locals[v];
  } else {
    while(dstackptr==(cstackptr?cstack[cstackptr-1].dstackptr:0)) debugger("Data stack underflow.");
    return dstack[dstackptr-1];
  }
}

static inline void vstore(byte v,zint s) {
  if(v>15) {
    while((v<<1)+global-31>=purbot) debugger("Access to global variable beyond PURBOT.");
    put16((v<<1)+global-32,s);
  } else if(v) {
    while(!cstackptr || v>cstack[cstackptr-1].numlocals) debugger("Access to undefined local variable.");
    cstack[cstackptr-1].locals[v]=s;
  } else {
    while(dstackptr==(cstackptr?cstack[cstackptr-1].dstackptr:0)) debugger("Data stack underflow.");
    dstack[dstackptr-1]=s;
  }
}

static inline void call_func(void) {
  int x,y;
  if(!*args) {
    store(0);
    return;
  }
  while(cstackptr==256) debugger("Call stack overflow.");
  cstack[cstackptr].dstackptr=dstackptr;
  cstack[cstackptr].pc=pc;
  pc=((sint32)(uzint)args[0])<<1;
  while(pc>=game_size) debugger("Call to function outside of story file.");
  while(mem[pc]>15) debugger("Too many local variables.");
  while(pc+(mem[pc]<<1)+1>=game_size) debugger("Call to function outside of story file.");
  y=cstack[cstackptr].numlocals=mem[pc++];
  for(x=1;x<=y;x++) {
    cstack[cstackptr].locals[x]=get16(pc);
    pc+=2;
  }
  for(x=1;x<argn;x++) cstack[cstackptr].locals[x]=args[x];
  cstackptr++;
  update_highwater();
}

static void call_return(zint r) {
  while(!cstackptr) debugger("Returning from START.");
  pc=cstack[--cstackptr].pc;
  dstackptr=cstack[cstackptr].dstackptr;
  store(r);
}

static void branch(int predicate) {
  sint32 amount=0;
  int x;
  pc_limit(1);
  x=mem[pc++];
  if(x&64) amount=x&63; else amount=((x&63)<<8)|mem[pc++];
  if(amount&0x2000) amount-=0x4000;
  if((x&128) && !predicate) return;
  if(!(x&128) && predicate) return;
  if(amount&~1) {
    pc+=amount-2;
  } else {
    call_return(amount);
  }
}

#define check_object_header(x) while(object+61+(x)*9>=purbot || !(x)) debugger("Object is zero or beyond PURBOT.")
#define check_object_header_r(x) while(object+61+(x)*9>=endlod || !(x)) debugger("Object is zero or beyond ENDLOD.")
#define loc_slot(x) mem[object+57+(x)*9]
#define next_slot(x) mem[object+58+(x)*9]
#define first_slot(x) mem[object+59+(x)*9]

static void print_sdesc(byte o) {
  sint32 a;
  check_object_header_r(o);
  a=get16(object+60+o*9);
  zprint(a+1);
}

static void update_status_line(void) {
  if((get16(global)&~255) || !get16(global)) warn("Invalid object number for status line.");
}

static void remove_object(zint o) {
  int x,y;
  while(!o || (o&~255)) debugger("Attempting to move an invalid object.");
  check_object_header(o);
  x=loc_slot(o);
  if(!x) return;
  check_object_header(x);
  if(first_slot(x)==o) {
    first_slot(x)=next_slot(o);
  } else {
    x=first_slot(x);
    for(;;) {
      check_object_header_r(x);
      y=x;
      x=next_slot(x);
      if(x==o) break;
      while(!x) debugger("An object contains something it doesn't know about.");
    }
    check_object_header(y);
    next_slot(y)=next_slot(o);
  }
  loc_slot(o)=next_slot(o)=0;
}

static void insert_object(zint o1,zint o2) {
  while(!o1 || !o2 || ((o1|o2)&~255)) debugger("Attempting to move an invalid object.");
  check_object_header(o1);
  check_object_header(o2);
  loc_slot(o1)=o2;
  next_slot(o1)=first_slot(o2);
  first_slot(o2)=o1;
}

static uzint property_table(zint o,zint p) {
  sint32 a;
  zint t=64;
  while(!o || (o&~255)) debugger("Attempting access a property of an invalid object.");
  check_object_header_r(o);
  a=get16(object+60+o*9);
  if(!p) return a;
  a+=(mem[a]<<1)+1;
  for(;;) {
    while(a<1 || a>=endlod) debugger("Property table is beyond ENDLOD.");
    if(!mem[a]) return 0;
    if((mem[a]&31)>=t) debugger("Property table is not in order.");
    t=mem[a]&31;
    if(t==p) {
      while(a+1+(mem[a]>>5)>=endlod) debugger("Property data is beyond ENDLOD.");
      return a+1;
    }
    a+=(mem[a]>>5)+2;
  }
}

static uzint vocab_lookup(char*buf,int len) {
  uzint w1,w2;
  sint32 a=vocab+mem[vocab]+4;
  byte zw[6]={5,5,5,5,5,5};
  int i,j,k;
  int nb=mem[vocab+mem[vocab]+1];
  int nw=get16(vocab+mem[vocab]+2);
  for(i=j=0;i<len && j<6;i++) {
    k=buf[i];
    if(k>='a' && k<='z') {
      zw[j++]=k+6-'a';
    } else if(strchr(cs2ascii,k)) {
      zw[j++]=5;
      if(j!=6) zw[j++]=strchr(cs2ascii,k)-cs2ascii;
    } else {
      zw[j++]=5;
      if(j!=6) zw[j++]=6;
      if(j!=6) zw[j++]=k>>5;
      if(j!=6) zw[j++]=k&31;
    }
  }
  w1=(zw[0]<<10)|(zw[1]<<5)|zw[2];
  w2=(zw[3]<<10)|(zw[4]<<5)|zw[5]|0x8000;
  for(i=0;i<nw;i++) {
    if((uzint)get16(a)==w1 && (uzint)get16(a+2)==w2) return a;
    a+=nb;
  }
  return 0;
}

static void do_read_line(void) {
  char buf[256];
  int i;
  sint32 t1=(uzint)args[0];
  sint32 t2=(uzint)args[1];
  uzint v;
  dumptext();
  update_status_line();
  lines=screen_rows-3;
  while(t1<64 || t1+mem[t1]>=purbot) debugger("Input buffer is not writable.");
  while(t2<64 || t2+mem[t2]*4>=purbot) debugger("Lex buffer is not writable.");
  if(!mem[t1]) warn("Input buffer may be too small.");
  while(!mem[t2]) debugger("Lex buffer is too small.");
  reread_header_flags();
  for(;;) {
    fgets(buf,256,stdin);
    if(escape && *buf==escape) exec_escape(buf+1); else break;
  }
  if(script && (hdrflags()&1)) fprintf(script,"%s\n",buf);
  for(i=0;buf[i];i++) {
    if(buf[i]>='A' && buf[i]<='Z') buf[i]+='a'-'A';
    else if(buf[i]==13 || buf[i]==10) buf[i]=0;
    else if(buf[i]==9) buf[i]=32;
  }
  buf[mem[t1]]=0;
  strcpy(mem+t1+1,buf);
  mem[t2+1]=0;
  mem[t2+5]=1;
  for(i=0;;i++) {
    if(buf[i]==32 || !buf[i]) {
      if(i+1==mem[t2+mem[t2+1]*4+5]) {
        mem[t2+mem[t2+1]*4+5]=i+2;
      } else {
        v=vocab_lookup(buf+mem[t2+mem[t2+1]*4+5]-1,mem[t2+mem[t2+1]*4+4]=i+1-mem[t2+mem[t2+1]*4+5]);
        put16(t2+mem[t2+1]*4+2,v);
        mem[t2+1]++;
        if(mem[t2+1]==mem[t2]) break;
        mem[t2+mem[t2+1]*4+5]=i+2;
      }
      if(!buf[i]) break;
    } else if(sib[buf[i]]) {
      if(i+1!=mem[t2+mem[t2+1]*4+5]) {
        v=vocab_lookup(buf+mem[t2+mem[t2+1]*4+5]-1,mem[t2+mem[t2+1]*4+4]=i+1-mem[t2+mem[t2+1]*4+5]);
        put16(t2+mem[t2+1]*4+2,v);
        mem[t2+1]++;
        if(mem[t2+1]==mem[t2]) break;
        mem[t2+mem[t2+1]*4+5]=i+2;
      }
      v=vocab_lookup(buf+mem[t2+mem[t2+1]*4+5]-1,mem[t2+mem[t2+1]*4+4]=i+2-mem[t2+mem[t2+1]*4+5]);
      put16(t2+mem[t2+1]*4+2,v);
      mem[t2+1]++;
      if(mem[t2+1]==mem[t2]) break;
      mem[t2+mem[t2+1]*4+5]=i+2;
    }
  }
}

#define FileData(d,k) if(saving) fwrite(d,k,sizeof(*(d)),fp); else fread(d,k,sizeof(*(d)),fp);

static int sav_res(int saving) {
  FILE*fp=savegame;
  char buf[256];
  sint32 x,y,z;
  if(textptr) warn("Saving while text in buffer.");
  if(restrict_saving) return 0;
  linebreak();
  begin_system_text();
  if(!fp) {
    printf("%s? ",saving?"Save":"Restore");
    fgets(buf,256,stdin);
    for(x=0;x<256;x++) if(buf[x]<=32) buf[x]=0;
    if(*buf<=32) return 0;
    fp=fopen(buf,saving?"wb":"rb");
    if(!fp) {
      puts("Cannot open save game file.");
      return 0;
    }
  } else {
    rewind(fp);
  }
  if(!saving) {
    y=get16(2);
    z=get16(16);
  }
  FileData(mem,purbot);
  FileData(dstack,dstackptr);
  FileData(cstack,cstackptr);
  FileData(&pc,1);
  FileData(&cstackptr,1);
  FileData(&dstackptr,1);
  FileData(&instpc,1);
  if(!saving) {
    if(y!=get16(2)) printf("ZORKID does not match. (Trying anyways)\n");
    put16(16,z);
  }
  if(savegame) rewind(fp); else fclose(fp);
  pc_changed=1;
  return 1;
}

static inline int verify_checksum(void) {
  uzint p=get16(26);
  uzint c=get16(28);
  sint32 t=((sint32)p)<<1;
  sint32 a=64;
  rewind(story);
  fseek(story,64,SEEK_SET);
  while(a<purbot) a++,c-=fgetc(story);
  while(a<t) c-=mem[a++];
  return !(c&0xFFFF);
}

#define Case_2OP(x) case x: case x+32: case x+64: case x+96: case x+192:
#define Case_1OP(x) case x: case x+16: case x+32:
#define Case_0OP(x) case x:
#define Case_EXT(x) case x:

static void execute(void) {
  int x,y,z;
  sint32 p;
  while(setjmp(exception_buffer));
  pc_changed=0;
  for(;;) {
    instpc=pc; // used for debugging
    for(y=0;y<32;y++) if(pcbreak[y]==pc) debugger("Breakpoint reached.");
    pc_limit(1);
    x=mem[pc++];
    ++profile[x];
    if(x&128) {
      if(x&64) {
        // EXT
        pc_limit(1);
        z=mem[pc++];
        argn=4;
        for(y=0;y<4;y++) {
          if((z&0xC0)==0xC0) {
            if(argn==4) argn=y;
          } else {
            if(argn!=4) debugger("The no more operands signal is early.");
            switch(z&0xC0) {
              case 0x00:
                pc_limit(2);
                args[y]=get16(pc);
                pc+=2;
                break;
              case 0x40:
                pc_limit(1);
                args[y]=mem[pc++];
                break;
              case 0x80:
                pc_limit(1);
                args[y]=fetch(mem[pc++]);
                break;
            }
          }
          z<<=2;
        }
      } else if(x<176) {
        // 1OP or 0OP
        argn=1;
        switch(x&0x30) {
          case 0x00:
            pc_limit(2);
            args[0]=get16(pc);
            pc+=2;
            break;
          case 0x10:
            pc_limit(1);
            args[0]=mem[pc++];
            break;
          case 0x20:
            pc_limit(1);
            args[0]=fetch(mem[pc++]);
            break;
          case 0x30:
            argn=0;
            break;
        }
      }
    } else {
      // 2OP
      pc_limit(2);
      argn=2;
      args[0]=x&64?fetch(mem[pc]):mem[pc];
      args[1]=x&32?fetch(mem[pc+1]):mem[pc+1];
      pc+=2;
    }
    if(tracing_execution) printf("<%d %d %d %d %d %d %d %d>",instpc,pc,x,argn,args[0],args[1],args[2],args[3]);
    if(instbreak[x]) debugger("Instruction breakpoint reached.");
    switch(x) {
      Case_2OP(1) //EQUAL?
        operand_limit(2,4);
        x=args[0]==args[1];
        if(argn>2) x|=args[0]==args[2];
        if(argn>3) x|=args[0]==args[3];
        branch(x);
        break;
      Case_2OP(2) //LESS?
        operand_limit(2,2);
        branch(args[0]<args[1]);
        break;
      Case_2OP(3) //GRTR?
        operand_limit(2,2);
        branch(args[0]>args[1]);
        break;
      Case_2OP(4) //DLESS?
        operand_limit(2,2);
        if(args[0]&~255) debugger("Variable number out of range.");
        if(vfetch(args[0])==-32768) debugger("Arithmetic overflow.");
        vstore(args[0],vfetch(args[0])-1);
        branch(vfetch(args[0])<args[1]);
        break;
      Case_2OP(5) //IGRTR?
        operand_limit(2,2);
        if(args[0]&~255) debugger("Variable number out of range.");
        if(vfetch(args[0])==32767) debugger("Arithmetic overflow.");
        vstore(args[0],vfetch(args[0])+1);
        branch(vfetch(args[0])>args[1]);
        break;
      Case_2OP(6) //IN?
        operand_limit(2,2);
        while((args[0]|args[1])&~255) debugger("Invalid object number.");
        check_object_header_r(args[0]);
        branch(loc_slot(args[0])==args[1]);
        break;
      Case_2OP(7) //BTST
        operand_limit(2,2);
        branch((args[0]&args[1])==args[1]);
        break;
      Case_2OP(8) //BOR
        operand_limit(2,2);
        store(args[0]|args[1]);
        break;
      Case_2OP(9) //BAND
        operand_limit(2,2);
        store(args[0]&args[1]);
        break;
      Case_2OP(10) //FSET?
        operand_limit(2,2);
        while(args[1]<0 || args[1]>31) debugger("Invalid flag number.");
        while(args[0]&~255) debugger("Invalid object.");
        check_object_header_r(args[0]);
        p=object+53+args[0]*9+((args[1]>>3)^(mem[1]&1));
        branch(mem[p]&(128>>(args[1]&7)));
        break;
      Case_2OP(11) //FSET
        operand_limit(2,2);
        while(args[1]<0 || args[1]>31) debugger("Invalid flag number.");
        while(args[0]&~255) debugger("Invalid object.");
        check_object_header(args[0]);
        p=object+53+args[0]*9+((args[1]>>3)^(mem[1]&1));
        mem[p]|=128>>(args[1]&7);
        break;
      Case_2OP(12) //FCLEAR
        operand_limit(2,2);
        while(args[1]<0 || args[1]>31) debugger("Invalid flag number.");
        while(args[0]&~255) debugger("Invalid object.");
        check_object_header(args[0]);
        p=object+53+args[0]*9+((args[1]>>3)^(mem[1]&1));
        mem[p]&=~(128>>(args[1]&7));
        break;
      Case_2OP(13) //SET
        operand_limit(2,2);
        if(args[0]&~255) debugger("Variable number out of range.");
        vstore(args[0],args[1]);
        break;
      Case_2OP(14) //MOVE
        operand_limit(2,2);
        remove_object(args[0]);
        if(!args[1]) warn("Moving an object into zero.");
        if(args[1]) insert_object(args[0],args[1]);
        break;
      Case_2OP(15) //GET
        operand_limit(2,2);
        if(args[1]<0) warn("Offset to GET is negative.");
        p=((uzint)args[0])+(((sint32)args[1])<<1);
        if(p&~0xFFFFL) warn("Offset to GET outside of range.");
        p&=0xFFFF;
        while(p+1>=game_size) debugger("Table address out of range.");
        store(get16(p));
        break;
      Case_2OP(16) //GETB
        operand_limit(2,2);
        if(args[1]<0) warn("Offset to GETB is negative.");
        p=((uzint)args[0])+args[1];
        if(p&~0xFFFFL) warn("Offset to GETB outside of range.");
        p&=0xFFFF;
        while(p>=game_size) debugger("Table address out of range.");
        store(mem[p]);
        break;
      Case_2OP(17) //GETP
        operand_limit(2,2);
        while(args[1]<1 || args[1]>31) debugger("Invalid property number.");
        p=property_table(args[0],args[1]);
        if(p) {
          while(mem[p-1]&0xC0) debugger("Property is too big.");
          store(mem[p-1]&0x20?get16(p):mem[p]);
        } else {
          p=object+((args[1]-1)<<1);
          store(get16(p));
        }
        break;
      Case_2OP(18) //GETPT
        operand_limit(2,2);
        while(args[1]<1 || args[1]>31) debugger("Invalid property number.");
        store(property_table(args[0],args[1]));
        break;
      Case_2OP(19) //NEXTP
        operand_limit(2,2);
        while(args[1]<0 || args[1]>31) debugger("Invalid property number.");
        p=property_table(args[0],args[1]);
        while(!p) debugger("Property does not exist.");
        if(args[1]) {
          --p;
          p+=(mem[p]>>5)+2;
        } else {
          p+=(mem[p]<<1)+1;
        }
        while(p>=endlod) debugger("Property table is beyond ENDLOD.");
        store(mem[p]&31);
        break;
      Case_2OP(20) //ADD
        operand_limit(2,2);
        store(args[0]+args[1]);
        break;
      Case_2OP(21) //SUB
        operand_limit(2,2);
        store(args[0]-args[1]);
        break;
      Case_2OP(22) //MUL
        operand_limit(2,2);
        p=args[0]*args[1];
        if(warnings && (p<-32768 || p>32767)) debugger("Arithmetic overflow.");
        store(p);
        break;
      Case_2OP(23) //DIV
        operand_limit(2,2);
        if(args[0]==-32768 && args[1]==-1) debugger("Arithmetic overflow.");
        while(!args[1]) debugger("Division by zero.");
        store(args[0]/args[1]);
        break;
      Case_2OP(24) //MOD
        operand_limit(2,2);
        while(!args[1]) debugger("Division by zero.");
        store(args[0]%args[1]);
        break;
      Case_1OP(128) //ZERO?
        branch(!*args);
        break;
      Case_1OP(129) //NEXT?
        check_object_header_r(*args);
        store(next_slot(*args));
        branch(next_slot(*args));
        break;
      Case_1OP(130) //FIRST?
        check_object_header_r(*args);
        store(first_slot(*args));
        branch(first_slot(*args));
        break;
      Case_1OP(131) //LOC
        check_object_header_r(*args);
        store(loc_slot(*args));
        break;
      Case_1OP(132) //PTSIZE
        p=(uzint)(*args);
        while(p>endlod) debugger("Accessing property size beyond ENDLOD.");
        while(!p) debugger("Accessing property size at address zero.");
        store((mem[p-1]>>5)+1);
        break;
      Case_1OP(133) //INC
        if(*args&~255) debugger("Variable number out of range.");
        vstore(*args,vfetch(*args)+1);
        break;
      Case_1OP(134) //DEC
        if(*args&~255) debugger("Variable number out of range.");
        vstore(*args,vfetch(*args)-1);
        break;
      Case_1OP(135) //PRINTB
        reread_header_flags();
        zprint((uzint)(*args));
        break;
      Case_1OP(137) //REMOVE
        remove_object(args[0]);
        break;
      Case_1OP(138) //PRINTD
        reread_header_flags();
        if(!*args || (*args&~255)) debugger("Object number out of range.");
        print_sdesc(*args);
        break;
      Case_1OP(139) //RETURN
        call_return(*args);
        break;
      Case_1OP(140) //JUMP
        pc+=*args-2;
        break;
      Case_1OP(141) //PRINT
        reread_header_flags();
        zprint(((sint32)(uzint)(*args))<<1);
        break;
      Case_1OP(142) //VALUE
        if(*args&~255) debugger("Variable number out of range.");
        store(vfetch(*args));
        break;
      Case_0OP(176) //RTRUE
        call_return(1);
        break;
      Case_0OP(177) //RFALSE
        call_return(0);
        break;
      Case_0OP(178) //PRINTI
        reread_header_flags();
        pc=zprint(pc);
        break;
      Case_0OP(179) //PRINTR
        reread_header_flags();
        zprint(pc);
        zprintchar(13);
        call_return(1);
        break;
      Case_0OP(180) //NOOP
        // Does nothing
        break;
      Case_0OP(181) //SAVE
        branch(sav_res(1));
        break;
      Case_0OP(182) //RESTORE
        branch(sav_res(0));
        break;
      Case_0OP(183) //RESTART
        dumptext();
        if(debug_on_quit) debugger("Game is restarting.");
        //if(is_ansi) printf("\e[%sm\e[2J\n",sgr_story);
        puts("\n\n\n\n\n\n\n\n\n\n\n\n\n");
        restart(1);
        break;
      Case_0OP(184) //RSTACK
        call_return(pop());
        break;
      Case_0OP(185) //FSTACK
        pop();
        break;
      Case_0OP(186) //QUIT
        dumptext();
        if(debug_on_quit) debugger("Game is terminating.");
        //if(is_ansi) printf("\e[%sm",sgr_story);
        putchar(10);
        exit(0);
        break;
      Case_0OP(187) //CRLF
        reread_header_flags();
        zprintchar(13);
        break;
      Case_0OP(188) //USL
        update_status_line();
        break;
      Case_0OP(189) //VERIFY
        branch(verify_checksum());
        break;
      Case_EXT(224) //CALL
        operand_limit(1,4);
        call_func();
        break;
      Case_EXT(225) //PUT
        operand_limit(3,3);
        if(args[1]<0) warn("Offset to PUT is negative.");
        p=((uzint)args[0])+(((sint32)args[1])<<1);
        if(p&~0xFFFFL) warn("Offset to GET outside of range.");
        p&=0xFFFF;
        while(p+1>=purbot) debugger("Table address out of range.");
        put16(p,args[2]);
        break;
      Case_EXT(226) //PUTB
        operand_limit(3,3);
        if(args[1]<0) warn("Offset to PUTB is negative.");
        p=((uzint)args[0])+args[1];
        if(p&~0xFFFFL) warn("Offset to PUTB outside of range.");
        p&=0xFFFF;
        while(p>=purbot) debugger("Table address out of range.");
        if(args[2]&~255) warn("Attempting to write too large value by use of PUTB.");
        mem[p]=args[2];
        break;
      Case_EXT(227) //PUTP
        operand_limit(3,3);
        while(args[1]<1 || args[1]>31) debugger("Invalid property number.");
        p=property_table(args[0],args[1]);
        if(p) {
          while(mem[p-1]&0xC0) debugger("Property is too big.");
          while(p+1+(mem[p]>>5)>=purbot) debugger("Property data is beyond PURBOT.");
          if(mem[p-1]&32) {
            put16(p,args[2]);
          } else {
            if(args[2]&~255) warn("Property value doesn't fit in 8-bits.");
            mem[p]=args[2];
          }
        } else {
          debugger("Attempting to write a nonexistent property.");
        }
        break;
      Case_EXT(228) //READ
        operand_limit(2,2);
        do_read_line();
        counter=0;
        break;
      Case_EXT(229) //PRINTC
        operand_limit(1,1);
        reread_header_flags();
        zprintchar(*args);
        break;
      Case_EXT(230) //PRINTN
        operand_limit(1,1);
        reread_header_flags();
        zprintnum(*args);
        break;
      Case_EXT(231) //RANDOM
        operand_limit(1,1);
        store(randgen(*args));
        break;
      Case_EXT(232) //PUSH
        operand_limit(1,1);
        push(*args);
        break;
      Case_EXT(233) //POP
        operand_limit(1,1);
        if(*args&~255) debugger("Variable number out of range.");
        vstore(*args,pop());
        break;
      Case_EXT(234) //SPLIT
        operand_limit(1,1);
        // Does nothing
        break;
      Case_EXT(235) //SCREEN
        operand_limit(1,1);
        // Does nothing
        break;
      default:
        for(;;) debugger("Invalid instruction opcode.");
    }
    ++counter;
    if(inststep && !--inststep) {
      instpc=pc;
      debugger("Stepped.");
    }
  }
}

static inline void parse_arguments(int argc,char**argv) {
  int n,i,c;
  for(n=1;n<argc;n++) {
    if(argv[n][0]=='-') {
      if(argv[n][1]=='-') {
        if(n!=argc-2) fatal("usage: zorkmid [options] <story>");
        *option_data=argv[n];
        return;
      } else {
        for(i=1;c=argv[n][i];i++) {
          if(option_set[c]==3) {
            fatal("Option is already specified.");
          } else if(option_set[argv[n][i]]==2) {
            if(argv[n][i+1] || n==argc-1) fatal("Option requires a value.");
            option_set[c]=3;
            option_data[c]=argv[++n];
            if(!argv[n][0]) fatal("Option requires a value.");
            break;
          } else if(option_set[argv[n][i]]==1) {
            option_set[c]=3;
            option_data[c]="";
          } else if(option_set[argv[n][i]]==0) {
            fatal("Unrecognized switch.");
          }
        }
      }
    } else {
      *option_data=argv[n];
      if(n!=argc-1) fatal("usage: zorkmid [options] <story>");
      return;
    }
  }
  fatal("usage: zorkmid [options] <story>");
}

static void prepare_debugger(const char*filename) {
  FILE*fp=fopen(filename,"r");
  char buf[256];
  if(!fp) fatal("Cannot open debugging file.");
  while(fgets(buf,256,fp)) debugger_exec(buf);
  fclose(fp);
}

static void check_story(void) {
  rewind(story);
  if(fgetc(story)!=3) fatal("Version byte is wrong.");
  if(savegame) {
    rewind(savegame);
    fgetc(savegame);
    fgetc(savegame);
    fgetc(story);
    if(fgetc(story)!=fgetc(savegame)) fatal("Save file does not have a matching ZORKID.");
    if(fgetc(story)!=fgetc(savegame)) fatal("Save file does not have a matching ZORKID.");
  }
  fseek(story,0,SEEK_END);
  if(ftell(story)<65) fatal("Story file is too small.");
  if(ftell(story)>0x20000) fatal("Story file is too big.");
}

static void restart(int midgame) {
  sint32 x=0;
  int oldflags;
  if(midgame) oldflags=get16(16);
  rewind(story);
  fseek(story,0,SEEK_END);
  x=ftell(story);
  if(x>0x20000 || x<=0) fatal("Something is seriously wrong with this story file.");
  game_size=x;
  rewind(story);
  fread(mem,1,x,story);
  for(x=0;x<=screen_rows;x++) putchar(10);
  textptr=curpos=0;
  lines=screen_rows-1;
  mem[1]&=3;
  mem[1]|=is_tandy<<3;
  dstackptr=cstackptr=0;
  debugptr=instpc=pc=get16(6);
  endlod=get16(4);
  vocab=get16(8);
  object=get16(10);
  global=get16(12);
  purbot=get16(14);
  fwords=get16(24);
  if(midgame) put16(16,oldflags);
  if(endlod>game_size) fatal("Preload size is larger than file size.");
  if(purbot>endlod) fatal("Writable area is larger than total preload size.");
  if(endlod<65) fatal("Preload size is too small.");
  if(vocab>=endlod || global>=endlod || fwords>=endlod) fatal("Important tables are outside of preloaded area.");
  if(get16(vocab+mem[vocab]+2)<=0) warn("Number of vocabulary entries is zero or negative.");
  if(vocab+mem[vocab]+(get16(vocab+mem[vocab]+2)-1)*mem[vocab+mem[vocab]+1]+4>=endlod) fatal("Vocabulary doesn't fit in core.");
  if(get16(vocab+mem[vocab]+2)!=1 && mem[vocab+mem[vocab]+1]<4) fatal("Size of vocabulary entry is too small.");
  for(x=0;x<mem[vocab];x++) if(mem[vocab+x+1]<128) sib[mem[vocab+x+1]]=1;
  if(sib[32] || sib[13] || sib[10]) warn("Found improper self-inserting-break characters.");
  counter=0;
}

int main(int argc,char**argv) {
  option_set['b']=1; // Break into debugger before game starts
  option_set['c']=2; // Number of columns
  option_set['e']=2; // Set escape character
  option_set['g']=2; // Set name of save game file
  option_set['h']=1; // Halt immediately in case of any error
  option_set['l']=2; // Number of lines
  option_set['q']=1; // Break into debugger on RESTART or QUIT command
  option_set['r']=1; // Disable support for save games
  option_set['s']=2; // File to write transcripts to
  option_set['t']=1; // Set Tandy bit
  option_set['w']=1; // Enable warnings
  option_set['x']=1; // Do not wrap text
  option_set['y']=2; // Set file containing debugging info
  option_set['C']=1; // Trace all Z-characters read
  option_set['F']=1; // Put braces around frequent words as they are output
  option_set['L']=1; // Do not pause at the end of each screen
  option_set['R']=2; // Set input stream for random numbers
  option_set['T']=1; // Trace execution
  parse_arguments(argc,argv);
  if(option_data['g'] && option_data['r']) fatal("Options -g and -r are conflicting.");
  if(!(story=fopen(*option_data,"rb"))) fatal("Cannot open story file.");
  if(option_data['s']) if(!(script=fopen(option_data['s'],"w"))) fatal("Cannot open script file.");
  if(option_data['R']) if(!(randstream=fopen(option_data['R'],"rb"))) fatal("Cannot open random numbers file.");
  if(option_data['g']) {
    if(!(savegame=fopen(option_data['g'],"r+b"))) {
      if(!(savegame=fopen(option_data['g'],"wb"))) fatal("Cannot open save game file.");
      rewind(story);
      fputc(fgetc(story)+1,savegame); // deliberately damage it so you know it isn't valid
      fputc(fgetc(story),savegame);
      fputc(fgetc(story),savegame);
      fputc(fgetc(story),savegame);
      fclose(savegame);
      if(!(savegame=fopen(option_data['g'],"r+b"))) fatal("Cannot open save game file.");
    }
  }
  if(option_data['e']) {
    escape=option_data['e'][0];
    if(option_data['e'][1]) fatal("Escape code must be a single ASCII character.");
  }
  if(option_data['r']) restrict_saving=1;
  if(option_data['t']) is_tandy=1;
  if(option_data['w']) warnings=1;
  if(option_data['q']) debug_on_quit=1;
  if(option_data['h']) halt_on_error=1;
  if(option_data['F']) embrace_fwords=1;
  if(option_data['T']) tracing_execution=1;
  if(option_data['C']) tracing_zchars=1;
  check_story();
  if(option_data['y']) prepare_debugger(option_data['y']);
  if(option_data['l']) screen_rows=strtol(option_data['l'],0,0);
  if(option_data['c']) screen_columns=strtol(option_data['c'],0,0);
  if(option_data['L']) screen_rows=-1;
  if(option_data['x']) screen_columns=-1;
  if(!screen_rows) screen_rows=strtol(getenv("LINES")?:"25",0,10)?:25;
  if(!screen_columns) screen_columns=strtol(getenv("COLUMNS")?:"79",0,10)?:79;
  if(screen_rows<4) screen_rows=4;
  if(screen_columns<12) screen_columns=12;
  restart(0);
  if(option_data['b']) debugger("Requested by command-line switch.");
  execute();
  fclose(story);
  if(script) fclose(script);
  if(savegame) fclose(savegame);
  return 0;
}
