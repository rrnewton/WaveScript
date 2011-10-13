//WSLIBDEPS: -lpthread -lgc
#define USE_BOEHM


#include "/export/home/meiyuan/smartnet/code/WaveScript/src/linked_lib/wsc2.h"
#include "sys/types.h"
#include "sys/socket.h"
#include "netinet/in.h"
#include "socket_wrappers.c"
#include "pthread.h"
#include "unix_wrappers.c"
#include "stdio.h"



struct tuptyp_586 {
  double fld1;
  double fld2;
  double fld3;
  double fld4;
  int64_t fld5;
  int64_t fld6;
  char* fld7;
  int64_t fld8;
} 
;


int stopalltimers = 0;
char* tmpconstlift_189 ;
char* tmpconstlift_188 ;
char* tmpconstlift_187 ;
char* tmpconstlift_186 ;
char* tmpconstlift_185 ;
char* tmpconstlift_184 ;
char* tmpconstlift_183 ;
char* tmpconstlift_182 ;
char* tmpconstlift_181 ;
char* tmpconstlift_180 ;
char* tmpconstlift_179 ;
char* tmpconstlift_178 ;
char* tmpconstlift_177 ;
char* tmpconstlift_176 ;
char* tmpconstlift_175 ;
char* tmpconstlift_174 ;
char* tmpconstlift_173 ;
char* tmpconstlift_172 ;
char* tmpconstlift_171 ;
char* tmpconstlift_170 ;
char* tmpconstlift_169 ;
char* tmpconstlift_168 ;
char* tmpconstlift_167 ;
char* tmpconstlift_166 ;
char* tmpconstlift_165 ;
char* tmpconstlift_164 ;
char* tmpconstlift_163 ;
char* tmpconstlift_162 ;
char* tmpconstlift_161 ;
char* tmpconstlift_160 ;
char* tmpconstlift_159 ;
char* tmpconstlift_158 ;
char* tmpconstlift_157 ;
void tmpsmp_572(char x_145); // Iter prototype
void wsq_printer_1(struct tuptyp_586 x_11); // Iter prototype
char Unix_fflush_9 ;
FILE* stdout_8 ;
void socket_in_2(uint8_t* buf_14); // Iter prototype
void socket_in_raw_3(uint8_t* arg_829);
void socket_in_raw_4(char __19); // Iter prototype
char spawn_socket_client_helper_16 ;
void socket_in_raw_5(char __37); // Iter prototype
char Unix_read_bytes_34 ;
uint8_t* tempbuf_33 ;
char Unix_puts_err_32 ;
char wsexit_31 ;
char shutdown_sockets_30 ;
char Unix_get_errno_29 ;
char Unix_ewouldblock_28 ;
char poll_socket_client_ready_port_27 ;
int header_26 ;
ws_bool_t warned_25 ;
int wouldblock_24 ;
int sockfd_23 ;
ws_bool_t have_header_22 ;
ws_bool_t connected_21 ;
DECLARE_WORKER(0, char, BASE)
DECLARE_WORKER(1, char, socket_in_raw_5)
DECLARE_WORKER(2, char, socket_in_raw_4)
DECLARE_WORKER(3, uint8_t*, socket_in_raw_3)
DECLARE_WORKER(4, uint8_t*, socket_in_2)
DECLARE_WORKER(5, struct tuptyp_586, wsq_printer_1)
DECLARE_WORKER(6, char, tmpsmp_572)

void tmpsmp_572(char x_145) {
  char ___VIRTQUEUE____146;
  GRAB_WRITEFIFO(BASE);
  printf("%s", tmpconstlift_189);
  printf("%s", tmpconstlift_188);
  printf("%s", tmpconstlift_187);
  EMIT(((char)0), char, BASE);
  RELEASE_WRITEFIFO(BASE);
} 

void wsq_printer_1(struct tuptyp_586 x_11) {
  char ___VIRTQUEUE____10;
  GRAB_WRITEFIFO(tmpsmp_572);
  printf("%s", tmpconstlift_175);
  printf("%s", tmpconstlift_174);
  printf("%s", tmpconstlift_173);
  {
    double tmpsmp_566 ;
     tmpsmp_566 = (x_11.fld1);
    printf("%.15g", tmpsmp_566);
  } 
  printf("%s", tmpconstlift_172);
  printf("%s", tmpconstlift_171);
  {
    double tmpsmp_564 ;
     tmpsmp_564 = (x_11.fld2);
    printf("%.15g", tmpsmp_564);
  } 
  printf("%s", tmpconstlift_170);
  printf("%s", tmpconstlift_169);
  {
    double tmpsmp_562 ;
     tmpsmp_562 = (x_11.fld3);
    printf("%.15g", tmpsmp_562);
  } 
  printf("%s", tmpconstlift_168);
  printf("%s", tmpconstlift_167);
  {
    double tmpsmp_560 ;
     tmpsmp_560 = (x_11.fld4);
    printf("%.15g", tmpsmp_560);
  } 
  printf("%s", tmpconstlift_166);
  printf("%s", tmpconstlift_165);
  {
    int64_t tmpsmp_558 ;
     tmpsmp_558 = (x_11.fld5);
    printf("%lld", tmpsmp_558);
  } 
  printf("%s", tmpconstlift_164);
  printf("%s", tmpconstlift_163);
  {
    int64_t tmpsmp_556 ;
     tmpsmp_556 = (x_11.fld6);
    printf("%lld", tmpsmp_556);
  } 
  printf("%s", tmpconstlift_162);
  printf("%s", tmpconstlift_161);
  {
    char* tmpsmp_554 ;
    {
      char* tmprc_590 ;
       tmprc_590 = (x_11.fld7);
       tmpsmp_554 = tmprc_590;
    } 
    printf("%s", tmpsmp_554);
  } 
  printf("%s", tmpconstlift_160);
  printf("%s", tmpconstlift_159);
  {
    int64_t tmpsmp_552 ;
     tmpsmp_552 = (x_11.fld8);
    printf("%lld", tmpsmp_552);
  } 
  printf("%s", tmpconstlift_158);
  printf("%s", tmpconstlift_157);
  {
    char ignored_valInEffect_573 ;
    fflush(stdout_8);
     ignored_valInEffect_573 = ((char)0);
  } 
  #ifdef WS_DISJOINT_HEAPS
  {
      // Copy & Release enqueued FIFO contents.
      int i;
      int pending = wsfifo_pending(& tmpsmp_572_queue); 
      for(i=0; i < pending ; i++) { 
        void* ptr = wsfifo_recheck(& tmpsmp_572_queue);
        // COPY IT
    
        wsfifo_release_one(& tmpsmp_572_queue);
      }
  } 
  #endif
  RELEASE_WRITEFIFO(tmpsmp_572);
} 

void socket_in_2(uint8_t* buf_14) {
  char ___VIRTQUEUE____13;
  GRAB_WRITEFIFO(wsq_printer_1);
  {
    int offset_239 ;
    {
      int tmprc_596 ;
       tmprc_596 = 0;
       offset_239 = tmprc_596;
    } 
    {
      int tmpsmp_508 ;
       tmpsmp_508 = (offset_239 + 8);
      offset_239 = tmpsmp_508;
    } 
    {
      int tmpsmp_506 ;
       tmpsmp_506 = (offset_239 - 8);
      {
        double tupfld_247 ;
         tupfld_247 = (*((double *)(buf_14 + tmpsmp_506))) /* type_unsafe_read */;
        {
          int tmpsmp_512 ;
           tmpsmp_512 = (offset_239 + 8);
          offset_239 = tmpsmp_512;
        } 
        {
          int tmpsmp_510 ;
           tmpsmp_510 = (offset_239 - 8);
          {
            double tupfld_246 ;
             tupfld_246 = (*((double *)(buf_14 + tmpsmp_510))) /* type_unsafe_read */;
            {
              int tmpsmp_516 ;
               tmpsmp_516 = (offset_239 + 8);
              offset_239 = tmpsmp_516;
            } 
            {
              int tmpsmp_514 ;
               tmpsmp_514 = (offset_239 - 8);
              {
                double tupfld_245 ;
                 tupfld_245 = (*((double *)(buf_14 + tmpsmp_514))) /* type_unsafe_read */;
                {
                  int tmpsmp_520 ;
                   tmpsmp_520 = (offset_239 + 8);
                  offset_239 = tmpsmp_520;
                } 
                {
                  int tmpsmp_518 ;
                   tmpsmp_518 = (offset_239 - 8);
                  {
                    double tupfld_244 ;
                     tupfld_244 = (*((double *)(buf_14 + tmpsmp_518))) /* type_unsafe_read */;
                    {
                      int tmpsmp_524 ;
                       tmpsmp_524 = (offset_239 + 8);
                      offset_239 = tmpsmp_524;
                    } 
                    {
                      int tmpsmp_522 ;
                       tmpsmp_522 = (offset_239 - 8);
                      {
                        int64_t tupfld_243 ;
                         tupfld_243 = (*((int64_t *)(buf_14 + tmpsmp_522))) /* type_unsafe_read */;
                        {
                          int tmpsmp_528 ;
                           tmpsmp_528 = (offset_239 + 8);
                          offset_239 = tmpsmp_528;
                        } 
                        {
                          int tmpsmp_526 ;
                           tmpsmp_526 = (offset_239 - 8);
                          {
                            int64_t tupfld_242 ;
                             tupfld_242 = (*((int64_t *)(buf_14 + tmpsmp_526))) /* type_unsafe_read */;
                            {
                              int tmpsmp_532 ;
                               tmpsmp_532 = (offset_239 + 4);
                              offset_239 = tmpsmp_532;
                            } 
                            {
                              int tmpsmp_530 ;
                               tmpsmp_530 = (offset_239 - 4);
                              {
                                int len_248 ;
                                 len_248 = (*((int *)(buf_14 + tmpsmp_530))) /* type_unsafe_read */;
                                {
                                  char* arr_249 ;
                                  {
                                    char* tmprc_595 ;
                                    int* arrtmp_830 = (int*)0;
                                    if (len_248 > 0) {
                                      arrtmp_830 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * len_248) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
                                      SETARRLEN(arrtmp_830, len_248);
                                    } 
                                     tmprc_595 = (char*)arrtmp_830;
                                     arr_249 = tmprc_595;
                                  } 
                                  {
                                    int tmpsmp_534 ;
                                     tmpsmp_534 = (len_248 - 1);
                                    {
                                       int ind_250;
                                      for (ind_250 = 0; ind_250 <= tmpsmp_534; ind_250++) {
                                        {
                                          int tmpsmp_538 ;
                                           tmpsmp_538 = (offset_239 + 1);
                                          offset_239 = tmpsmp_538;
                                        } 
                                        {
                                          int tmpsmp_536 ;
                                           tmpsmp_536 = (offset_239 - 1);
                                          {
                                            char tmpsmp_540 ;
                                             tmpsmp_540 = (*((char *)(buf_14 + tmpsmp_536))) /* type_unsafe_read */;
                                            arr_249[ind_250] = tmpsmp_540;
                                          } 
                                        } 
                                      } 
                                    } 
                                  } 
                                  {
                                    int tmpsmp_546 ;
                                     tmpsmp_546 = (offset_239 + 8);
                                    offset_239 = tmpsmp_546;
                                  } 
                                  {
                                    int tmpsmp_544 ;
                                     tmpsmp_544 = (offset_239 - 8);
                                    {
                                      int64_t tupfld_240 ;
                                       tupfld_240 = (*((int64_t *)(buf_14 + tmpsmp_544))) /* type_unsafe_read */;
                                      {
                                        struct tuptyp_586 ob_15 ;
                                        {
                                          struct tuptyp_586 tmprc_592 = {tupfld_247, tupfld_246, tupfld_245, tupfld_244, tupfld_243, tupfld_242, arr_249, tupfld_240};
                                           ob_15 = tmprc_592;
                                        } 
                                        EMIT(ob_15, struct tuptyp_586, wsq_printer_1);
                                      } 
                                    } 
                                  } 
                                } 
                              } 
                            } 
                          } 
                        } 
                      } 
                    } 
                  } 
                } 
              } 
            } 
          } 
        } 
      } 
    } 
  } 
  #ifdef WS_DISJOINT_HEAPS
  {
      // Copy & Release enqueued FIFO contents.
      int i;
      int pending = wsfifo_pending(& wsq_printer_1_queue); 
      for(i=0; i < pending ; i++) { 
        void* ptr = wsfifo_recheck(& wsq_printer_1_queue);
        // COPY IT
    
        wsfifo_release_one(& wsq_printer_1_queue);
      }
  } 
  #endif
  RELEASE_WRITEFIFO(wsq_printer_1);
} 

// merge operator: 
void socket_in_raw_3(uint8_t* arg_829) {
  GRAB_WRITEFIFO(socket_in_2);
  EMIT(arg_829, uint8_t*, socket_in_2);
  RELEASE_WRITEFIFO(socket_in_2);
} 

void socket_in_raw_4(char __19) {
  char ___VIRTQUEUE____18;
  GRAB_WRITEFIFO(socket_in_raw_3);
  {
    int64_t ignored_valInEffect_574 ;
     ignored_valInEffect_574 = spawn_socket_client_helper(tmpconstlift_176, 9877) /* foreign app */ ;
  } 
  RELEASE_WRITEFIFO(socket_in_raw_3);
} 

void socket_in_raw_5(char __37) {
  char ___VIRTQUEUE____36;
  GRAB_WRITEFIFO(socket_in_raw_3);
  {
    ws_bool_t tmpsmp_488 ;
     tmpsmp_488 = (sockfd_23 == 0);
    {
      char ignored_valInEffect_582 ;
      if (tmpsmp_488) {
        {
          int tmpsmp_498 ;
           tmpsmp_498 = poll_socket_client_ready_port(9877) /* foreign app */ ;
          sockfd_23 = tmpsmp_498;
        } 
        {
          ws_bool_t tmpsmp_490 ;
           tmpsmp_490 = (sockfd_23 == 0);
          {
            ws_bool_t tmpsmp_492 ;
             tmpsmp_492 = !(tmpsmp_490);
            {
              char tmpsmp_500 ;
              if (tmpsmp_492) {
                connected_21 = TRUE;
                {
                  int tmpsmp_494 ;
                   tmpsmp_494 = ws_EWOULDBLOCK() /* foreign app */ ;
                  wouldblock_24 = tmpsmp_494;
                   tmpsmp_500 = ((char)0);
                } 
              } else {
                 tmpsmp_500 = ((char)0);
              }
               ignored_valInEffect_582 = tmpsmp_500;
            } 
          } 
        } 
      } else {
         ignored_valInEffect_582 = ((char)0);
      }
    } 
  } 
  {
    char ignored_valInEffect_581 ;
    if (connected_21) {
      {
        int len_38 ;
        if (have_header_22) {
           len_38 = header_26;
        } else {
          {
            int arg_60 ;
             arg_60 = sockfd_23;
            {
              int rd_50 ;
               rd_50 = read(arg_60, tempbuf_33, 4) /* foreign app */ ;
              {
                ws_bool_t tmpsmp_256 ;
                 tmpsmp_256 = (rd_50 == -1);
                {
                  ws_bool_t tmpsmp_362 ;
                  if (tmpsmp_256) {
                    {
                      int code_56 ;
                       code_56 = get_errno() /* foreign app */ ;
                      {
                        ws_bool_t tmpsmp_258 ;
                         tmpsmp_258 = (code_56 == wouldblock_24);
                        {
                          ws_bool_t tmpsmp_306 ;
                          if (tmpsmp_258) {
                             tmpsmp_306 = FALSE;
                          } else {
                            {
                              char* strarr1_202 ;
                              {
                                char* tmprc_628 ;
                                int* arrtmp_826 = (int*)0;
                                if (100 > 0) {
                                  arrtmp_826 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 100) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
                                  SETARRLEN(arrtmp_826, 100);
                                } 
                                char* str_824 = (char*)arrtmp_826;
                                int realsize_825 = snprintf(str_824, 100, "%d", code_56);
                                if (realsize_825 >= 100) { printf("Error, show overflowed fixed length 100 buffer\n"); exit(-1); }
                                SETARRLEN(str_824, realsize_825 + 1); /* set virtual length */ 
                                 tmprc_628 = str_824;
                                 strarr1_202 = tmprc_628;
                              } 
                              {
                                char* strarr2_203 ;
                                {
                                  char* tmprc_630 ;
                                   tmprc_630 = tmpconstlift_185;
                                   strarr2_203 = tmprc_630;
                                } 
                                {
                                  int tmpsmp_260 ;
                                   tmpsmp_260 = ARRLEN(strarr1_202);
                                  {
                                    int len_206 ;
                                     len_206 = (tmpsmp_260 - 1);
                                    {
                                      int tmpsmp_262 ;
                                       tmpsmp_262 = ARRLEN(strarr2_203);
                                      {
                                        int tmpsmp_264 ;
                                         tmpsmp_264 = (len_206 + tmpsmp_262);
                                        {
                                          char* appendresult_207 ;
                                          {
                                            char* tmprc_636 ;
                                            int* arrtmp_823 = (int*)0;
                                            if (tmpsmp_264 > 0) {
                                              arrtmp_823 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * tmpsmp_264) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
                                              SETARRLEN(arrtmp_823, tmpsmp_264);
                                            } 
                                             tmprc_636 = (char*)arrtmp_823;
                                             appendresult_207 = tmprc_636;
                                          } 
                                          {
                                            int tmpsmp_276 ;
                                             tmpsmp_276 = (len_206 - 1);
                                            {
                                               int i_204;
                                              for (i_204 = 0; i_204 <= tmpsmp_276; i_204++) {
                                                {
                                                  char tmpsmp_278 ;
                                                   tmpsmp_278 = strarr1_202[i_204];
                                                  appendresult_207[i_204] = tmpsmp_278;
                                                } 
                                              } 
                                            } 
                                          } 
                                          {
                                            int tmpsmp_266 ;
                                             tmpsmp_266 = ARRLEN(strarr2_203);
                                            {
                                              int tmpsmp_268 ;
                                               tmpsmp_268 = (tmpsmp_266 - 1);
                                              {
                                                 int i_205;
                                                for (i_205 = 0; i_205 <= tmpsmp_268; i_205++) {
                                                  {
                                                    int tmpsmp_270 ;
                                                     tmpsmp_270 = (len_206 + i_205);
                                                    {
                                                      char tmpsmp_272 ;
                                                       tmpsmp_272 = strarr2_203[i_205];
                                                      appendresult_207[tmpsmp_270] = tmpsmp_272;
                                                    } 
                                                  } 
                                                } 
                                              } 
                                            } 
                                          } 
                                          {
                                            int tmpsmp_282 ;
                                             tmpsmp_282 = ARRLEN(tmpconstlift_186);
                                            {
                                              int len_212 ;
                                               len_212 = (tmpsmp_282 - 1);
                                              {
                                                int tmpsmp_284 ;
                                                 tmpsmp_284 = ARRLEN(appendresult_207);
                                                {
                                                  int tmpsmp_286 ;
                                                   tmpsmp_286 = (len_212 + tmpsmp_284);
                                                  {
                                                    char* appendresult_213 ;
                                                    {
                                                      char* tmprc_642 ;
                                                      int* arrtmp_822 = (int*)0;
                                                      if (tmpsmp_286 > 0) {
                                                        arrtmp_822 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * tmpsmp_286) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
                                                        SETARRLEN(arrtmp_822, tmpsmp_286);
                                                      } 
                                                       tmprc_642 = (char*)arrtmp_822;
                                                       appendresult_213 = tmprc_642;
                                                    } 
                                                    {
                                                      int tmpsmp_298 ;
                                                       tmpsmp_298 = (len_212 - 1);
                                                      {
                                                         int i_210;
                                                        for (i_210 = 0; i_210 <= tmpsmp_298; i_210++) {
                                                          {
                                                            char tmpsmp_300 ;
                                                             tmpsmp_300 = tmpconstlift_186[i_210];
                                                            appendresult_213[i_210] = tmpsmp_300;
                                                          } 
                                                        } 
                                                      } 
                                                    } 
                                                    {
                                                      int tmpsmp_288 ;
                                                       tmpsmp_288 = ARRLEN(appendresult_207);
                                                      {
                                                        int tmpsmp_290 ;
                                                         tmpsmp_290 = (tmpsmp_288 - 1);
                                                        {
                                                           int i_211;
                                                          for (i_211 = 0; i_211 <= tmpsmp_290; i_211++) {
                                                            {
                                                              int tmpsmp_292 ;
                                                               tmpsmp_292 = (len_212 + i_211);
                                                              {
                                                                char tmpsmp_294 ;
                                                                 tmpsmp_294 = appendresult_207[i_211];
                                                                appendresult_213[tmpsmp_292] = tmpsmp_294;
                                                              } 
                                                            } 
                                                          } 
                                                        } 
                                                      } 
                                                    } 
                                                    wserror_builtin(appendresult_213);
                                                    /* BOTTOM CTRL PATH */
                                                  } 
                                                } 
                                              } 
                                            } 
                                          } 
                                        } 
                                      } 
                                    } 
                                  } 
                                } 
                              } 
                            } 
                          }
                           tmpsmp_362 = tmpsmp_306;
                        } 
                      } 
                    } 
                  } else {
                    {
                      ws_bool_t tmpsmp_308 ;
                       tmpsmp_308 = (rd_50 == 4);
                      {
                        ws_bool_t tmpsmp_360 ;
                        if (tmpsmp_308) {
                           tmpsmp_360 = TRUE;
                        } else {
                          {
                            ws_bool_t tmpsmp_310 ;
                             tmpsmp_310 = !(warned_25);
                            {
                              char ignored_valInEffect_580 ;
                              if (tmpsmp_310) {
                                {
                                  char* strarr1_190 ;
                                  {
                                    char* tmprc_623 ;
                                     tmprc_623 = tmpconstlift_183;
                                     strarr1_190 = tmprc_623;
                                  } 
                                  {
                                    char* strarr2_191 ;
                                    {
                                      char* tmprc_622 ;
                                       tmprc_622 = tmpconstlift_182;
                                       strarr2_191 = tmprc_622;
                                    } 
                                    {
                                      int tmpsmp_312 ;
                                       tmpsmp_312 = ARRLEN(strarr1_190);
                                      {
                                        int len_194 ;
                                         len_194 = (tmpsmp_312 - 1);
                                        {
                                          int tmpsmp_314 ;
                                           tmpsmp_314 = ARRLEN(strarr2_191);
                                          {
                                            int tmpsmp_316 ;
                                             tmpsmp_316 = (len_194 + tmpsmp_314);
                                            {
                                              char* appendresult_195 ;
                                              {
                                                char* tmprc_621 ;
                                                int* arrtmp_828 = (int*)0;
                                                if (tmpsmp_316 > 0) {
                                                  arrtmp_828 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * tmpsmp_316) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
                                                  SETARRLEN(arrtmp_828, tmpsmp_316);
                                                } 
                                                 tmprc_621 = (char*)arrtmp_828;
                                                 appendresult_195 = tmprc_621;
                                              } 
                                              {
                                                int tmpsmp_328 ;
                                                 tmpsmp_328 = (len_194 - 1);
                                                {
                                                   int i_192;
                                                  for (i_192 = 0; i_192 <= tmpsmp_328; i_192++) {
                                                    {
                                                      char tmpsmp_330 ;
                                                       tmpsmp_330 = strarr1_190[i_192];
                                                      appendresult_195[i_192] = tmpsmp_330;
                                                    } 
                                                  } 
                                                } 
                                              } 
                                              {
                                                int tmpsmp_318 ;
                                                 tmpsmp_318 = ARRLEN(strarr2_191);
                                                {
                                                  int tmpsmp_320 ;
                                                   tmpsmp_320 = (tmpsmp_318 - 1);
                                                  {
                                                     int i_193;
                                                    for (i_193 = 0; i_193 <= tmpsmp_320; i_193++) {
                                                      {
                                                        int tmpsmp_322 ;
                                                         tmpsmp_322 = (len_194 + i_193);
                                                        {
                                                          char tmpsmp_324 ;
                                                           tmpsmp_324 = strarr2_191[i_193];
                                                          appendresult_195[tmpsmp_322] = tmpsmp_324;
                                                        } 
                                                      } 
                                                    } 
                                                  } 
                                                } 
                                              } 
                                              {
                                                int tmpsmp_334 ;
                                                 tmpsmp_334 = ARRLEN(tmpconstlift_184);
                                                {
                                                  int len_200 ;
                                                   len_200 = (tmpsmp_334 - 1);
                                                  {
                                                    int tmpsmp_336 ;
                                                     tmpsmp_336 = ARRLEN(appendresult_195);
                                                    {
                                                      int tmpsmp_338 ;
                                                       tmpsmp_338 = (len_200 + tmpsmp_336);
                                                      {
                                                        char* appendresult_201 ;
                                                        {
                                                          char* tmprc_616 ;
                                                          int* arrtmp_827 = (int*)0;
                                                          if (tmpsmp_338 > 0) {
                                                            arrtmp_827 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * tmpsmp_338) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
                                                            SETARRLEN(arrtmp_827, tmpsmp_338);
                                                          } 
                                                           tmprc_616 = (char*)arrtmp_827;
                                                           appendresult_201 = tmprc_616;
                                                        } 
                                                        {
                                                          int tmpsmp_350 ;
                                                           tmpsmp_350 = (len_200 - 1);
                                                          {
                                                             int i_198;
                                                            for (i_198 = 0; i_198 <= tmpsmp_350; i_198++) {
                                                              {
                                                                char tmpsmp_352 ;
                                                                 tmpsmp_352 = tmpconstlift_184[i_198];
                                                                appendresult_201[i_198] = tmpsmp_352;
                                                              } 
                                                            } 
                                                          } 
                                                        } 
                                                        {
                                                          int tmpsmp_340 ;
                                                           tmpsmp_340 = ARRLEN(appendresult_195);
                                                          {
                                                            int tmpsmp_342 ;
                                                             tmpsmp_342 = (tmpsmp_340 - 1);
                                                            {
                                                               int i_199;
                                                              for (i_199 = 0; i_199 <= tmpsmp_342; i_199++) {
                                                                {
                                                                  int tmpsmp_344 ;
                                                                   tmpsmp_344 = (len_200 + i_199);
                                                                  {
                                                                    char tmpsmp_346 ;
                                                                     tmpsmp_346 = appendresult_195[i_199];
                                                                    appendresult_201[tmpsmp_344] = tmpsmp_346;
                                                                  } 
                                                                } 
                                                              } 
                                                            } 
                                                          } 
                                                        } 
                                                        {
                                                          char ignored_valInEffect_579 ;
                                                          puts_err(appendresult_201);
                                                           ignored_valInEffect_579 = ((char)0);
                                                        } 
                                                      } 
                                                    } 
                                                  } 
                                                } 
                                              } 
                                            } 
                                          } 
                                        } 
                                      } 
                                    } 
                                  } 
                                } 
                                {
                                  char ignored_valInEffect_578 ;
                                  shutdown_sockets();
                                   ignored_valInEffect_578 = ((char)0);
                                } 
                                {
                                  char tmpsmp_356 ;
                                  wsexit_fun(0);
                                   tmpsmp_356 = ((char)0);
                                   ignored_valInEffect_580 = tmpsmp_356;
                                } 
                              } else {
                                 ignored_valInEffect_580 = ((char)0);
                              }
                            } 
                          } 
                          warned_25 = TRUE;
                           tmpsmp_360 = FALSE;
                        }
                         tmpsmp_362 = tmpsmp_360;
                      } 
                    } 
                  }
                  {
                    int tmpsmp_370 ;
                    if (tmpsmp_362) {
                      have_header_22 = TRUE;
                      {
                        int offset_252 ;
                        {
                          int tmprc_653 ;
                           tmprc_653 = 0;
                           offset_252 = tmprc_653;
                        } 
                        {
                          int tmpsmp_366 ;
                           tmpsmp_366 = (offset_252 + 4);
                          offset_252 = tmpsmp_366;
                        } 
                        {
                          int tmpsmp_364 ;
                           tmpsmp_364 = (offset_252 - 4);
                          {
                            int tmp_51 ;
                             tmp_51 = (*((int *)(tempbuf_33 + tmpsmp_364))) /* type_unsafe_read */;
                            header_26 = tmp_51;
                            {
                              int tmpsmp_368 ;
                               tmpsmp_368 = header_26;
                               tmpsmp_370 = tmpsmp_368;
                            } 
                          } 
                        } 
                      } 
                    } else {
                       tmpsmp_370 = -1;
                    }
                     len_38 = tmpsmp_370;
                  } 
                } 
              } 
            } 
          } 
        }
        {
          ws_bool_t tmpsmp_372 ;
           tmpsmp_372 = (len_38 >= 0);
          {
            char tmpsmp_486 ;
            if (tmpsmp_372) {
              {
                uint8_t* buf_39 ;
                {
                  uint8_t* tmprc_661 ;
                  int* arrtmp_821 = (int*)0;
                  if (len_38 > 0) {
                    arrtmp_821 = (int*)((char*)WSCALLOC_SCALAR((sizeof(uint8_t) * len_38) + RCSIZE + ARRLENSIZE, 1) + RCSIZE + ARRLENSIZE);
                    SETARRLEN(arrtmp_821, len_38);
                  } 
                   tmprc_661 = (uint8_t*)arrtmp_821;
                   buf_39 = tmprc_661;
                } 
                {
                  int arg_49 ;
                   arg_49 = sockfd_23;
                  {
                    int rd_40 ;
                     rd_40 = read(arg_49, buf_39, len_38) /* foreign app */ ;
                    {
                      ws_bool_t tmpsmp_374 ;
                       tmpsmp_374 = (rd_40 == -1);
                      {
                        ws_bool_t tmpsmp_480 ;
                        if (tmpsmp_374) {
                          {
                            int code_45 ;
                             code_45 = get_errno() /* foreign app */ ;
                            {
                              ws_bool_t tmpsmp_376 ;
                               tmpsmp_376 = (code_45 == wouldblock_24);
                              {
                                ws_bool_t tmpsmp_424 ;
                                if (tmpsmp_376) {
                                   tmpsmp_424 = FALSE;
                                } else {
                                  {
                                    char* strarr1_226 ;
                                    {
                                      char* tmprc_686 ;
                                      int* arrtmp_818 = (int*)0;
                                      if (100 > 0) {
                                        arrtmp_818 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 100) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
                                        SETARRLEN(arrtmp_818, 100);
                                      } 
                                      char* str_816 = (char*)arrtmp_818;
                                      int realsize_817 = snprintf(str_816, 100, "%d", code_45);
                                      if (realsize_817 >= 100) { printf("Error, show overflowed fixed length 100 buffer\n"); exit(-1); }
                                      SETARRLEN(str_816, realsize_817 + 1); /* set virtual length */ 
                                       tmprc_686 = str_816;
                                       strarr1_226 = tmprc_686;
                                    } 
                                    {
                                      char* strarr2_227 ;
                                      {
                                        char* tmprc_688 ;
                                         tmprc_688 = tmpconstlift_180;
                                         strarr2_227 = tmprc_688;
                                      } 
                                      {
                                        int tmpsmp_378 ;
                                         tmpsmp_378 = ARRLEN(strarr1_226);
                                        {
                                          int len_230 ;
                                           len_230 = (tmpsmp_378 - 1);
                                          {
                                            int tmpsmp_380 ;
                                             tmpsmp_380 = ARRLEN(strarr2_227);
                                            {
                                              int tmpsmp_382 ;
                                               tmpsmp_382 = (len_230 + tmpsmp_380);
                                              {
                                                char* appendresult_231 ;
                                                {
                                                  char* tmprc_694 ;
                                                  int* arrtmp_815 = (int*)0;
                                                  if (tmpsmp_382 > 0) {
                                                    arrtmp_815 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * tmpsmp_382) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
                                                    SETARRLEN(arrtmp_815, tmpsmp_382);
                                                  } 
                                                   tmprc_694 = (char*)arrtmp_815;
                                                   appendresult_231 = tmprc_694;
                                                } 
                                                {
                                                  int tmpsmp_394 ;
                                                   tmpsmp_394 = (len_230 - 1);
                                                  {
                                                     int i_228;
                                                    for (i_228 = 0; i_228 <= tmpsmp_394; i_228++) {
                                                      {
                                                        char tmpsmp_396 ;
                                                         tmpsmp_396 = strarr1_226[i_228];
                                                        appendresult_231[i_228] = tmpsmp_396;
                                                      } 
                                                    } 
                                                  } 
                                                } 
                                                {
                                                  int tmpsmp_384 ;
                                                   tmpsmp_384 = ARRLEN(strarr2_227);
                                                  {
                                                    int tmpsmp_386 ;
                                                     tmpsmp_386 = (tmpsmp_384 - 1);
                                                    {
                                                       int i_229;
                                                      for (i_229 = 0; i_229 <= tmpsmp_386; i_229++) {
                                                        {
                                                          int tmpsmp_388 ;
                                                           tmpsmp_388 = (len_230 + i_229);
                                                          {
                                                            char tmpsmp_390 ;
                                                             tmpsmp_390 = strarr2_227[i_229];
                                                            appendresult_231[tmpsmp_388] = tmpsmp_390;
                                                          } 
                                                        } 
                                                      } 
                                                    } 
                                                  } 
                                                } 
                                                {
                                                  int tmpsmp_400 ;
                                                   tmpsmp_400 = ARRLEN(tmpconstlift_181);
                                                  {
                                                    int len_236 ;
                                                     len_236 = (tmpsmp_400 - 1);
                                                    {
                                                      int tmpsmp_402 ;
                                                       tmpsmp_402 = ARRLEN(appendresult_231);
                                                      {
                                                        int tmpsmp_404 ;
                                                         tmpsmp_404 = (len_236 + tmpsmp_402);
                                                        {
                                                          char* appendresult_237 ;
                                                          {
                                                            char* tmprc_700 ;
                                                            int* arrtmp_814 = (int*)0;
                                                            if (tmpsmp_404 > 0) {
                                                              arrtmp_814 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * tmpsmp_404) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
                                                              SETARRLEN(arrtmp_814, tmpsmp_404);
                                                            } 
                                                             tmprc_700 = (char*)arrtmp_814;
                                                             appendresult_237 = tmprc_700;
                                                          } 
                                                          {
                                                            int tmpsmp_416 ;
                                                             tmpsmp_416 = (len_236 - 1);
                                                            {
                                                               int i_234;
                                                              for (i_234 = 0; i_234 <= tmpsmp_416; i_234++) {
                                                                {
                                                                  char tmpsmp_418 ;
                                                                   tmpsmp_418 = tmpconstlift_181[i_234];
                                                                  appendresult_237[i_234] = tmpsmp_418;
                                                                } 
                                                              } 
                                                            } 
                                                          } 
                                                          {
                                                            int tmpsmp_406 ;
                                                             tmpsmp_406 = ARRLEN(appendresult_231);
                                                            {
                                                              int tmpsmp_408 ;
                                                               tmpsmp_408 = (tmpsmp_406 - 1);
                                                              {
                                                                 int i_235;
                                                                for (i_235 = 0; i_235 <= tmpsmp_408; i_235++) {
                                                                  {
                                                                    int tmpsmp_410 ;
                                                                     tmpsmp_410 = (len_236 + i_235);
                                                                    {
                                                                      char tmpsmp_412 ;
                                                                       tmpsmp_412 = appendresult_231[i_235];
                                                                      appendresult_237[tmpsmp_410] = tmpsmp_412;
                                                                    } 
                                                                  } 
                                                                } 
                                                              } 
                                                            } 
                                                          } 
                                                          wserror_builtin(appendresult_237);
                                                          /* BOTTOM CTRL PATH */
                                                        } 
                                                      } 
                                                    } 
                                                  } 
                                                } 
                                              } 
                                            } 
                                          } 
                                        } 
                                      } 
                                    } 
                                  } 
                                }
                                 tmpsmp_480 = tmpsmp_424;
                              } 
                            } 
                          } 
                        } else {
                          {
                            ws_bool_t tmpsmp_426 ;
                             tmpsmp_426 = (rd_40 == len_38);
                            {
                              ws_bool_t tmpsmp_478 ;
                              if (tmpsmp_426) {
                                 tmpsmp_478 = TRUE;
                              } else {
                                {
                                  ws_bool_t tmpsmp_428 ;
                                   tmpsmp_428 = !(warned_25);
                                  {
                                    char ignored_valInEffect_577 ;
                                    if (tmpsmp_428) {
                                      {
                                        char* strarr1_214 ;
                                        {
                                          char* tmprc_681 ;
                                           tmprc_681 = tmpconstlift_178;
                                           strarr1_214 = tmprc_681;
                                        } 
                                        {
                                          char* strarr2_215 ;
                                          {
                                            char* tmprc_680 ;
                                             tmprc_680 = tmpconstlift_177;
                                             strarr2_215 = tmprc_680;
                                          } 
                                          {
                                            int tmpsmp_430 ;
                                             tmpsmp_430 = ARRLEN(strarr1_214);
                                            {
                                              int len_218 ;
                                               len_218 = (tmpsmp_430 - 1);
                                              {
                                                int tmpsmp_432 ;
                                                 tmpsmp_432 = ARRLEN(strarr2_215);
                                                {
                                                  int tmpsmp_434 ;
                                                   tmpsmp_434 = (len_218 + tmpsmp_432);
                                                  {
                                                    char* appendresult_219 ;
                                                    {
                                                      char* tmprc_679 ;
                                                      int* arrtmp_820 = (int*)0;
                                                      if (tmpsmp_434 > 0) {
                                                        arrtmp_820 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * tmpsmp_434) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
                                                        SETARRLEN(arrtmp_820, tmpsmp_434);
                                                      } 
                                                       tmprc_679 = (char*)arrtmp_820;
                                                       appendresult_219 = tmprc_679;
                                                    } 
                                                    {
                                                      int tmpsmp_446 ;
                                                       tmpsmp_446 = (len_218 - 1);
                                                      {
                                                         int i_216;
                                                        for (i_216 = 0; i_216 <= tmpsmp_446; i_216++) {
                                                          {
                                                            char tmpsmp_448 ;
                                                             tmpsmp_448 = strarr1_214[i_216];
                                                            appendresult_219[i_216] = tmpsmp_448;
                                                          } 
                                                        } 
                                                      } 
                                                    } 
                                                    {
                                                      int tmpsmp_436 ;
                                                       tmpsmp_436 = ARRLEN(strarr2_215);
                                                      {
                                                        int tmpsmp_438 ;
                                                         tmpsmp_438 = (tmpsmp_436 - 1);
                                                        {
                                                           int i_217;
                                                          for (i_217 = 0; i_217 <= tmpsmp_438; i_217++) {
                                                            {
                                                              int tmpsmp_440 ;
                                                               tmpsmp_440 = (len_218 + i_217);
                                                              {
                                                                char tmpsmp_442 ;
                                                                 tmpsmp_442 = strarr2_215[i_217];
                                                                appendresult_219[tmpsmp_440] = tmpsmp_442;
                                                              } 
                                                            } 
                                                          } 
                                                        } 
                                                      } 
                                                    } 
                                                    {
                                                      int tmpsmp_452 ;
                                                       tmpsmp_452 = ARRLEN(tmpconstlift_179);
                                                      {
                                                        int len_224 ;
                                                         len_224 = (tmpsmp_452 - 1);
                                                        {
                                                          int tmpsmp_454 ;
                                                           tmpsmp_454 = ARRLEN(appendresult_219);
                                                          {
                                                            int tmpsmp_456 ;
                                                             tmpsmp_456 = (len_224 + tmpsmp_454);
                                                            {
                                                              char* appendresult_225 ;
                                                              {
                                                                char* tmprc_674 ;
                                                                int* arrtmp_819 = (int*)0;
                                                                if (tmpsmp_456 > 0) {
                                                                  arrtmp_819 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * tmpsmp_456) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
                                                                  SETARRLEN(arrtmp_819, tmpsmp_456);
                                                                } 
                                                                 tmprc_674 = (char*)arrtmp_819;
                                                                 appendresult_225 = tmprc_674;
                                                              } 
                                                              {
                                                                int tmpsmp_468 ;
                                                                 tmpsmp_468 = (len_224 - 1);
                                                                {
                                                                   int i_222;
                                                                  for (i_222 = 0; i_222 <= tmpsmp_468; i_222++) {
                                                                    {
                                                                      char tmpsmp_470 ;
                                                                       tmpsmp_470 = tmpconstlift_179[i_222];
                                                                      appendresult_225[i_222] = tmpsmp_470;
                                                                    } 
                                                                  } 
                                                                } 
                                                              } 
                                                              {
                                                                int tmpsmp_458 ;
                                                                 tmpsmp_458 = ARRLEN(appendresult_219);
                                                                {
                                                                  int tmpsmp_460 ;
                                                                   tmpsmp_460 = (tmpsmp_458 - 1);
                                                                  {
                                                                     int i_223;
                                                                    for (i_223 = 0; i_223 <= tmpsmp_460; i_223++) {
                                                                      {
                                                                        int tmpsmp_462 ;
                                                                         tmpsmp_462 = (len_224 + i_223);
                                                                        {
                                                                          char tmpsmp_464 ;
                                                                           tmpsmp_464 = appendresult_219[i_223];
                                                                          appendresult_225[tmpsmp_462] = tmpsmp_464;
                                                                        } 
                                                                      } 
                                                                    } 
                                                                  } 
                                                                } 
                                                              } 
                                                              {
                                                                char ignored_valInEffect_576 ;
                                                                puts_err(appendresult_225);
                                                                 ignored_valInEffect_576 = ((char)0);
                                                              } 
                                                            } 
                                                          } 
                                                        } 
                                                      } 
                                                    } 
                                                  } 
                                                } 
                                              } 
                                            } 
                                          } 
                                        } 
                                      } 
                                      {
                                        char ignored_valInEffect_575 ;
                                        shutdown_sockets();
                                         ignored_valInEffect_575 = ((char)0);
                                      } 
                                      {
                                        char tmpsmp_474 ;
                                        wsexit_fun(0);
                                         tmpsmp_474 = ((char)0);
                                         ignored_valInEffect_577 = tmpsmp_474;
                                      } 
                                    } else {
                                       ignored_valInEffect_577 = ((char)0);
                                    }
                                  } 
                                } 
                                warned_25 = TRUE;
                                 tmpsmp_478 = FALSE;
                              }
                               tmpsmp_480 = tmpsmp_478;
                            } 
                          } 
                        }
                        {
                          char tmpsmp_484 ;
                          if (tmpsmp_480) {
                            have_header_22 = FALSE;
                            EMIT(buf_39, uint8_t*, socket_in_raw_3);
                             tmpsmp_484 = ((char)0);
                          } else {
                             tmpsmp_484 = ((char)0);
                          }
                           tmpsmp_486 = tmpsmp_484;
                        } 
                      } 
                    } 
                  } 
                } 
              } 
            } else {
               tmpsmp_486 = ((char)0);
            }
             ignored_valInEffect_581 = tmpsmp_486;
          } 
        } 
      } 
    } else {
       ignored_valInEffect_581 = ((char)0);
    }
  } 
  RELEASE_WRITEFIFO(socket_in_raw_3);
} 


void initState() {
  /* We may need to start up the Boehm GC or do other standard WS init: */ 
  wsInternalInit();
  TOTAL_WORKERS(7);
  // [2008.11.07] The static data gets allocated against a never-cleared ZCT: 
  #ifdef WS_THREADED 
  #ifdef WS_USE_ZCT 
   zct_t* zct = WSCALLOC(sizeof(zct_t), 1);
  #endif
  #endif
  REGISTER_WORKER(0, char, BASE);
  REGISTER_WORKER(1, char, socket_in_raw_5);
  REGISTER_WORKER(2, char, socket_in_raw_4);
  REGISTER_WORKER(3, uint8_t*, socket_in_raw_3);
  REGISTER_WORKER(4, uint8_t*, socket_in_2);
  REGISTER_WORKER(5, struct tuptyp_586, wsq_printer_1);
  REGISTER_WORKER(6, char, tmpsmp_572);
  {
    char* tmprc_746 ;
    int* arrtmp_812 = (int*)0;
    if (2 > 0) {
      arrtmp_812 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_812, 2);
    } 
    char* tmpchararr_811 = (char*)arrtmp_812;
    memcpy(tmpchararr_811, "(", 2);
     tmprc_746 = tmpchararr_811;
     tmpconstlift_189 = tmprc_746;
  } 
  {
    char* tmprc_745 ;
    int* arrtmp_810 = (int*)0;
    if (2 > 0) {
      arrtmp_810 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_810, 2);
    } 
    char* tmpchararr_809 = (char*)arrtmp_810;
    memcpy(tmpchararr_809, ")", 2);
     tmprc_745 = tmpchararr_809;
     tmpconstlift_188 = tmprc_745;
  } 
  {
    char* tmprc_744 ;
    int* arrtmp_808 = (int*)0;
    if (2 > 0) {
      arrtmp_808 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_808, 2);
    } 
    char* tmpchararr_807 = (char*)arrtmp_808;
    memcpy(tmpchararr_807, "\n", 2);
     tmprc_744 = tmpchararr_807;
     tmpconstlift_187 = tmprc_744;
  } 
  {
    char* tmprc_743 ;
    int* arrtmp_806 = (int*)0;
    if (44 > 0) {
      arrtmp_806 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 44) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_806, 44);
    } 
    char* tmpchararr_805 = (char*)arrtmp_806;
    memcpy(tmpchararr_805, "  <socket.ws> ERROR: read() returned errno ", 44);
     tmprc_743 = tmpchararr_805;
     tmpconstlift_186 = tmprc_743;
  } 
  {
    char* tmprc_742 ;
    int* arrtmp_804 = (int*)0;
    if (2 > 0) {
      arrtmp_804 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_804, 2);
    } 
    char* tmpchararr_803 = (char*)arrtmp_804;
    memcpy(tmpchararr_803, "\n", 2);
     tmprc_742 = tmpchararr_803;
     tmpconstlift_185 = tmprc_742;
  } 
  {
    char* tmprc_741 ;
    int* arrtmp_802 = (int*)0;
    if (15 > 0) {
      arrtmp_802 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 15) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_802, 15);
    } 
    char* tmpchararr_801 = (char*)arrtmp_802;
    memcpy(tmpchararr_801, "  <socket.ws> ", 15);
     tmprc_741 = tmpchararr_801;
     tmpconstlift_184 = tmprc_741;
  } 
  {
    char* tmprc_740 ;
    int* arrtmp_800 = (int*)0;
    if (11 > 0) {
      arrtmp_800 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 11) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_800, 11);
    } 
    char* tmpchararr_799 = (char*)arrtmp_800;
    memcpy(tmpchararr_799, "rd header:", 11);
     tmprc_740 = tmpchararr_799;
     tmpconstlift_183 = tmprc_740;
  } 
  {
    char* tmprc_739 ;
    int* arrtmp_798 = (int*)0;
    if (80 > 0) {
      arrtmp_798 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 80) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_798, 80);
    } 
    char* tmpchararr_797 = (char*)arrtmp_798;
    memcpy(tmpchararr_797, " Read wrong length, taking this as a signal that upstream is closed.  Exiting.\n", 80);
     tmprc_739 = tmpchararr_797;
     tmpconstlift_182 = tmprc_739;
  } 
  {
    char* tmprc_738 ;
    int* arrtmp_796 = (int*)0;
    if (44 > 0) {
      arrtmp_796 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 44) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_796, 44);
    } 
    char* tmpchararr_795 = (char*)arrtmp_796;
    memcpy(tmpchararr_795, "  <socket.ws> ERROR: read() returned errno ", 44);
     tmprc_738 = tmpchararr_795;
     tmpconstlift_181 = tmprc_738;
  } 
  {
    char* tmprc_737 ;
    int* arrtmp_794 = (int*)0;
    if (2 > 0) {
      arrtmp_794 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_794, 2);
    } 
    char* tmpchararr_793 = (char*)arrtmp_794;
    memcpy(tmpchararr_793, "\n", 2);
     tmprc_737 = tmpchararr_793;
     tmpconstlift_180 = tmprc_737;
  } 
  {
    char* tmprc_736 ;
    int* arrtmp_792 = (int*)0;
    if (15 > 0) {
      arrtmp_792 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 15) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_792, 15);
    } 
    char* tmpchararr_791 = (char*)arrtmp_792;
    memcpy(tmpchararr_791, "  <socket.ws> ", 15);
     tmprc_736 = tmpchararr_791;
     tmpconstlift_179 = tmprc_736;
  } 
  {
    char* tmprc_735 ;
    int* arrtmp_790 = (int*)0;
    if (12 > 0) {
      arrtmp_790 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 12) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_790, 12);
    } 
    char* tmpchararr_789 = (char*)arrtmp_790;
    memcpy(tmpchararr_789, "rd payload:", 12);
     tmprc_735 = tmpchararr_789;
     tmpconstlift_178 = tmprc_735;
  } 
  {
    char* tmprc_734 ;
    int* arrtmp_788 = (int*)0;
    if (80 > 0) {
      arrtmp_788 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 80) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_788, 80);
    } 
    char* tmpchararr_787 = (char*)arrtmp_788;
    memcpy(tmpchararr_787, " Read wrong length, taking this as a signal that upstream is closed.  Exiting.\n", 80);
     tmprc_734 = tmpchararr_787;
     tmpconstlift_177 = tmprc_734;
  } 
  {
    char* tmprc_733 ;
    int* arrtmp_786 = (int*)0;
    if (10 > 0) {
      arrtmp_786 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 10) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_786, 10);
    } 
    char* tmpchararr_785 = (char*)arrtmp_786;
    memcpy(tmpchararr_785, "localhost", 10);
     tmprc_733 = tmpchararr_785;
     tmpconstlift_176 = tmprc_733;
  } 
  {
    char* tmprc_732 ;
    int* arrtmp_784 = (int*)0;
    if (10 > 0) {
      arrtmp_784 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 10) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_784, 10);
    } 
    char* tmpchararr_783 = (char*)arrtmp_784;
    memcpy(tmpchararr_783, "NETSTRM: ", 10);
     tmprc_732 = tmpchararr_783;
     tmpconstlift_175 = tmprc_732;
  } 
  {
    char* tmprc_731 ;
    int* arrtmp_782 = (int*)0;
    if (2 > 0) {
      arrtmp_782 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_782, 2);
    } 
    char* tmpchararr_781 = (char*)arrtmp_782;
    memcpy(tmpchararr_781, "(", 2);
     tmprc_731 = tmpchararr_781;
     tmpconstlift_174 = tmprc_731;
  } 
  {
    char* tmprc_730 ;
    int* arrtmp_780 = (int*)0;
    if (5 > 0) {
      arrtmp_780 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_780, 5);
    } 
    char* tmpchararr_779 = (char*)arrtmp_780;
    memcpy(tmpchararr_779, "ASK=", 5);
     tmprc_730 = tmpchararr_779;
     tmpconstlift_173 = tmprc_730;
  } 
  {
    char* tmprc_729 ;
    int* arrtmp_778 = (int*)0;
    if (3 > 0) {
      arrtmp_778 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 3) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_778, 3);
    } 
    char* tmpchararr_777 = (char*)arrtmp_778;
    memcpy(tmpchararr_777, ", ", 3);
     tmprc_729 = tmpchararr_777;
     tmpconstlift_172 = tmprc_729;
  } 
  {
    char* tmprc_728 ;
    int* arrtmp_776 = (int*)0;
    if (9 > 0) {
      arrtmp_776 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 9) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_776, 9);
    } 
    char* tmpchararr_775 = (char*)arrtmp_776;
    memcpy(tmpchararr_775, "ASKSIZE=", 9);
     tmprc_728 = tmpchararr_775;
     tmpconstlift_171 = tmprc_728;
  } 
  {
    char* tmprc_727 ;
    int* arrtmp_774 = (int*)0;
    if (3 > 0) {
      arrtmp_774 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 3) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_774, 3);
    } 
    char* tmpchararr_773 = (char*)arrtmp_774;
    memcpy(tmpchararr_773, ", ", 3);
     tmprc_727 = tmpchararr_773;
     tmpconstlift_170 = tmprc_727;
  } 
  {
    char* tmprc_726 ;
    int* arrtmp_772 = (int*)0;
    if (5 > 0) {
      arrtmp_772 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_772, 5);
    } 
    char* tmpchararr_771 = (char*)arrtmp_772;
    memcpy(tmpchararr_771, "BID=", 5);
     tmprc_726 = tmpchararr_771;
     tmpconstlift_169 = tmprc_726;
  } 
  {
    char* tmprc_725 ;
    int* arrtmp_770 = (int*)0;
    if (3 > 0) {
      arrtmp_770 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 3) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_770, 3);
    } 
    char* tmpchararr_769 = (char*)arrtmp_770;
    memcpy(tmpchararr_769, ", ", 3);
     tmprc_725 = tmpchararr_769;
     tmpconstlift_168 = tmprc_725;
  } 
  {
    char* tmprc_724 ;
    int* arrtmp_768 = (int*)0;
    if (9 > 0) {
      arrtmp_768 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 9) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_768, 9);
    } 
    char* tmpchararr_767 = (char*)arrtmp_768;
    memcpy(tmpchararr_767, "BIDSIZE=", 9);
     tmprc_724 = tmpchararr_767;
     tmpconstlift_167 = tmprc_724;
  } 
  {
    char* tmprc_723 ;
    int* arrtmp_766 = (int*)0;
    if (3 > 0) {
      arrtmp_766 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 3) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_766, 3);
    } 
    char* tmpchararr_765 = (char*)arrtmp_766;
    memcpy(tmpchararr_765, ", ", 3);
     tmprc_723 = tmpchararr_765;
     tmpconstlift_166 = tmprc_723;
  } 
  {
    char* tmprc_722 ;
    int* arrtmp_764 = (int*)0;
    if (15 > 0) {
      arrtmp_764 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 15) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_764, 15);
    } 
    char* tmpchararr_763 = (char*)arrtmp_764;
    memcpy(tmpchararr_763, "EXCHTIMESTAMP=", 15);
     tmprc_722 = tmpchararr_763;
     tmpconstlift_165 = tmprc_722;
  } 
  {
    char* tmprc_721 ;
    int* arrtmp_762 = (int*)0;
    if (3 > 0) {
      arrtmp_762 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 3) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_762, 3);
    } 
    char* tmpchararr_761 = (char*)arrtmp_762;
    memcpy(tmpchararr_761, ", ", 3);
     tmprc_721 = tmpchararr_761;
     tmpconstlift_164 = tmprc_721;
  } 
  {
    char* tmprc_720 ;
    int* arrtmp_760 = (int*)0;
    if (14 > 0) {
      arrtmp_760 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 14) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_760, 14);
    } 
    char* tmpchararr_759 = (char*)arrtmp_760;
    memcpy(tmpchararr_759, "RECEIVEDTIME=", 14);
     tmprc_720 = tmpchararr_759;
     tmpconstlift_163 = tmprc_720;
  } 
  {
    char* tmprc_719 ;
    int* arrtmp_758 = (int*)0;
    if (3 > 0) {
      arrtmp_758 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 3) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_758, 3);
    } 
    char* tmpchararr_757 = (char*)arrtmp_758;
    memcpy(tmpchararr_757, ", ", 3);
     tmprc_719 = tmpchararr_757;
     tmpconstlift_162 = tmprc_719;
  } 
  {
    char* tmprc_718 ;
    int* arrtmp_756 = (int*)0;
    if (8 > 0) {
      arrtmp_756 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 8) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_756, 8);
    } 
    char* tmpchararr_755 = (char*)arrtmp_756;
    memcpy(tmpchararr_755, "SYMBOL=", 8);
     tmprc_718 = tmpchararr_755;
     tmpconstlift_161 = tmprc_718;
  } 
  {
    char* tmprc_717 ;
    int* arrtmp_754 = (int*)0;
    if (3 > 0) {
      arrtmp_754 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 3) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_754, 3);
    } 
    char* tmpchararr_753 = (char*)arrtmp_754;
    memcpy(tmpchararr_753, ", ", 3);
     tmprc_717 = tmpchararr_753;
     tmpconstlift_160 = tmprc_717;
  } 
  {
    char* tmprc_716 ;
    int* arrtmp_752 = (int*)0;
    if (11 > 0) {
      arrtmp_752 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 11) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_752, 11);
    } 
    char* tmpchararr_751 = (char*)arrtmp_752;
    memcpy(tmpchararr_751, "TIMESTAMP=", 11);
     tmprc_716 = tmpchararr_751;
     tmpconstlift_159 = tmprc_716;
  } 
  {
    char* tmprc_715 ;
    int* arrtmp_750 = (int*)0;
    if (2 > 0) {
      arrtmp_750 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_750, 2);
    } 
    char* tmpchararr_749 = (char*)arrtmp_750;
    memcpy(tmpchararr_749, ")", 2);
     tmprc_715 = tmpchararr_749;
     tmpconstlift_158 = tmprc_715;
  } 
  {
    char* tmprc_714 ;
    int* arrtmp_748 = (int*)0;
    if (2 > 0) {
      arrtmp_748 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_748, 2);
    } 
    char* tmpchararr_747 = (char*)arrtmp_748;
    memcpy(tmpchararr_747, "\n", 2);
     tmprc_714 = tmpchararr_747;
     tmpconstlift_157 = tmprc_714;
  } 
   Unix_fflush_9 = 0;
  {
    char tmpsmp_550 ;
     tmpsmp_550 = 0;
     stdout_8 = ws_get_stdout() /* foreign app */ ;
  } 
   spawn_socket_client_helper_16 = 0;
   Unix_read_bytes_34 = 0;
  {
    uint8_t* tmprc_603 ;
    int* arrtmp_813 = (int*)0;
    if (4 > 0) {
      arrtmp_813 = (int*)((char*)WSCALLOC_SCALAR((sizeof(uint8_t) * 4) + RCSIZE + ARRLENSIZE, 1) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_813, 4);
    } 
     tmprc_603 = (uint8_t*)arrtmp_813;
     tempbuf_33 = tmprc_603;
  } 
   Unix_puts_err_32 = 0;
   wsexit_31 = 0;
   shutdown_sockets_30 = 0;
   Unix_get_errno_29 = 0;
   Unix_ewouldblock_28 = 0;
   poll_socket_client_ready_port_27 = 0;
  {
    int tmprc_602 ;
     tmprc_602 = 0;
     header_26 = tmprc_602;
  } 
  {
    ws_bool_t tmprc_601 ;
     tmprc_601 = FALSE;
     warned_25 = tmprc_601;
  } 
  {
    int tmprc_600 ;
     tmprc_600 = 0;
     wouldblock_24 = tmprc_600;
  } 
  {
    int tmprc_599 ;
     tmprc_599 = 0;
     sockfd_23 = tmprc_599;
  } 
  {
    ws_bool_t tmprc_598 ;
     tmprc_598 = FALSE;
     have_header_22 = tmprc_598;
  } 
  {
    ws_bool_t tmprc_597 ;
     tmprc_597 = FALSE;
     connected_21 = tmprc_597;
  } 
  // We will never need to clear this ZCT, so we can throw it out:
  #ifdef WS_THREADED 
  #ifdef WS_USE_ZCT 
  WSFREE(zct);
  #endif
  #endif
  START_WORKERS();
} 

int main(int argc, char** argv) {
    initState();
  ws_parse_options(argc,argv); /* [2008.08.27] Moving to after initState */ 
  ws_bool_t dummy =TRUE;
  // Insert calls to those timers executing only once (with zero rates)
  GRAB_WRITEFIFO(socket_in_raw_4);
  EMIT(((char)0), char, socket_in_raw_4);
  RELEASE_WRITEFIFO(socket_in_raw_4);
  // Next, run in a timer loop indefinitely
  while(dummy && !stopalltimers) {
    VIRTTICK();
    int fired = 0;
    // And finally call the normal-timers on ticks where it is appropriate:
    // The timesteps where we fire are the only 'real' ones:
    if (1) {
      // Use do-while here to make sure that the negative guys go at least ONCE:
      do {
        // Insert calls to those (negative rate) timers that run max speed:
        // This substitutes for a call to WAIT_TICKS:
        GRAB_WRITEFIFO(socket_in_raw_5);
        EMIT(((char)0), char, socket_in_raw_5);
        RELEASE_WRITEFIFO(socket_in_raw_5);
      } 
      while (TIME_DEBT(1000.0) > 0.0);
    } 
  } 
  // We keep the main function going for other tuples to come through.
  while (print_queue_status()) { sleep(1); }
  return 0;
} 
