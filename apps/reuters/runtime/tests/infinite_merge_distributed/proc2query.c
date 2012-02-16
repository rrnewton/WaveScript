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



struct tuptyp_2067 {
  float fld1;
  char* fld2;
  float fld3;
  int fld4;
} 
;

struct Union2_1 {
  int tag;
  union {
    struct tuptyp_2067 Left_1;
    struct tuptyp_2067 Right_1;
  } 
   payload;
} 
;
enum Union2_1_enum {
  Left_1, Right_1
} 
;

int stopalltimers = 0;
char* tmpconstlift_1238 ;
char* tmpconstlift_1237 ;
char* tmpconstlift_1236 ;
char* tmpconstlift_1235 ;
char* tmpconstlift_1234 ;
char* tmpconstlift_1233 ;
char* tmpconstlift_1232 ;
char* tmpconstlift_1231 ;
char* tmpconstlift_1230 ;
char* tmpconstlift_1229 ;
char* tmpconstlift_1228 ;
char* tmpconstlift_1227 ;
char* tmpconstlift_1226 ;
char* tmpconstlift_1225 ;
char* tmpconstlift_1224 ;
char* tmpconstlift_1223 ;
char* tmpconstlift_1222 ;
char* tmpconstlift_1221 ;
char* tmpconstlift_1220 ;
char* tmpconstlift_1219 ;
char* tmpconstlift_1218 ;
char* tmpconstlift_1217 ;
char* tmpconstlift_1216 ;
char* tmpconstlift_1215 ;
char* tmpconstlift_1214 ;
char* tmpconstlift_1213 ;
char* tmpconstlift_1212 ;
char* tmpconstlift_1211 ;
char* tmpconstlift_1210 ;
char* tmpconstlift_1209 ;
char* tmpconstlift_1208 ;
char* tmpconstlift_1207 ;
char* tmpconstlift_1206 ;
char* tmpconstlift_1205 ;
char* tmpconstlift_1204 ;
char* tmpconstlift_1203 ;
char* tmpconstlift_1202 ;
char* tmpconstlift_1201 ;
char* tmpconstlift_1200 ;
char* tmpconstlift_1199 ;
char* tmpconstlift_1198 ;
char* tmpconstlift_1197 ;
char* tmpconstlift_1196 ;
char* tmpconstlift_1195 ;
char* tmpconstlift_1194 ;
char* tmpconstlift_1193 ;
char* tmpconstlift_1192 ;
char* tmpconstlift_1191 ;
char* tmpconstlift_1190 ;
char* tmpconstlift_1189 ;
char* tmpconstlift_1188 ;
char* tmpconstlift_1187 ;
char* tmpconstlift_1186 ;
char* tmpconstlift_1185 ;
char* tmpconstlift_1184 ;
char* tmpconstlift_1183 ;
char* tmpconstlift_1182 ;
char* tmpconstlift_1181 ;
char* tmpconstlift_1180 ;
char* tmpconstlift_1179 ;
char* tmpconstlift_1178 ;
char* tmpconstlift_1177 ;
char* tmpconstlift_1176 ;
char* tmpconstlift_1175 ;
char* tmpconstlift_1174 ;
char* tmpconstlift_1173 ;
char* tmpconstlift_1172 ;
char* tmpconstlift_1171 ;
char* tmpconstlift_1170 ;
char* tmpconstlift_1169 ;
char* tmpconstlift_1168 ;
char* tmpconstlift_1167 ;
char* tmpconstlift_1166 ;
char* tmpconstlift_1165 ;
char* tmpconstlift_1164 ;
char* tmpconstlift_1163 ;
char* tmpconstlift_1162 ;
char* tmpconstlift_1161 ;
char* tmpconstlift_1160 ;
char* tmpconstlift_1159 ;
char* tmpconstlift_1158 ;
char* tmpconstlift_1157 ;
char* tmpconstlift_1156 ;
char* tmpconstlift_1155 ;
char* tmpconstlift_1154 ;
char* tmpconstlift_1153 ;
char* tmpconstlift_1152 ;
char* tmpconstlift_1151 ;
char* tmpconstlift_1150 ;
char* tmpconstlift_1149 ;
char* tmpconstlift_1148 ;
char* tmpconstlift_1147 ;
char* tmpconstlift_1146 ;
char* tmpconstlift_1145 ;
char* tmpconstlift_1144 ;
char* tmpconstlift_1143 ;
char* tmpconstlift_1142 ;
char* tmpconstlift_1141 ;
char* tmpconstlift_1140 ;
char* tmpconstlift_1139 ;
char* tmpconstlift_1138 ;
char* tmpconstlift_1137 ;
char* tmpconstlift_1136 ;
char* tmpconstlift_1135 ;
char* tmpconstlift_1134 ;
char* tmpconstlift_1133 ;
char* tmpconstlift_1132 ;
char* tmpconstlift_1131 ;
char* tmpconstlift_1130 ;
char* tmpconstlift_1129 ;
char* tmpconstlift_1128 ;
char* tmpconstlift_1127 ;
char* tmpconstlift_1126 ;
char* tmpconstlift_1125 ;
char* tmpconstlift_1124 ;
char* tmpconstlift_1123 ;
char* tmpconstlift_1122 ;
char* tmpconstlift_1121 ;
char* tmpconstlift_1120 ;
char* tmpconstlift_1119 ;
char* tmpconstlift_1118 ;
char* tmpconstlift_1117 ;
char* tmpconstlift_1116 ;
char* tmpconstlift_1115 ;
char* tmpconstlift_1114 ;
char* tmpconstlift_1113 ;
char* tmpconstlift_1112 ;
char* tmpconstlift_1111 ;
char* tmpconstlift_1110 ;
char* tmpconstlift_1109 ;
char* tmpconstlift_1108 ;
char* tmpconstlift_1107 ;
char* tmpconstlift_1106 ;
char* tmpconstlift_1105 ;
char* tmpconstlift_1104 ;
char* tmpconstlift_1103 ;
char* tmpconstlift_1102 ;
char* tmpconstlift_1101 ;
char* tmpconstlift_1100 ;
char* tmpconstlift_1099 ;
char* tmpconstlift_1098 ;
char* tmpconstlift_1097 ;
char* tmpconstlift_1096 ;
char* tmpconstlift_1095 ;
char* tmpconstlift_1094 ;
char* tmpconstlift_1093 ;
char* tmpconstlift_1092 ;
char* tmpconstlift_1091 ;
char* tmpconstlift_1090 ;
char* tmpconstlift_1089 ;
char* tmpconstlift_1088 ;
char* tmpconstlift_1087 ;
char* tmpconstlift_1086 ;
char* tmpconstlift_1085 ;
char* tmpconstlift_1084 ;
char* tmpconstlift_1083 ;
char* tmpconstlift_1082 ;
char* tmpconstlift_1081 ;
char* tmpconstlift_1080 ;
char* tmpconstlift_1079 ;
char* tmpconstlift_1078 ;
char* tmpconstlift_1077 ;
char* tmpconstlift_1076 ;
char* tmpconstlift_1075 ;
char* tmpconstlift_1074 ;
char* tmpconstlift_1073 ;
char* tmpconstlift_1072 ;
char* tmpconstlift_1071 ;
char* tmpconstlift_1070 ;
char* tmpconstlift_1069 ;
char* tmpconstlift_1068 ;
char* tmpconstlift_1067 ;
char* tmpconstlift_1066 ;
char* tmpconstlift_1065 ;
char* tmpconstlift_1064 ;
char* tmpconstlift_1063 ;
char* tmpconstlift_1062 ;
char* tmpconstlift_1061 ;
char* tmpconstlift_1060 ;
char* tmpconstlift_1059 ;
char* tmpconstlift_1058 ;
char* tmpconstlift_1057 ;
char* tmpconstlift_1056 ;
char* tmpconstlift_1055 ;
char* tmpconstlift_1054 ;
char* tmpconstlift_1053 ;
char* tmpconstlift_1052 ;
char* tmpconstlift_1051 ;
char* tmpconstlift_1050 ;
char* tmpconstlift_1049 ;
char* tmpconstlift_1048 ;
char* tmpconstlift_1047 ;
char* tmpconstlift_1046 ;
char* tmpconstlift_1045 ;
char* tmpconstlift_1044 ;
char* tmpconstlift_1043 ;
char* tmpconstlift_1042 ;
char* tmpconstlift_1041 ;
char* tmpconstlift_1040 ;
char* tmpconstlift_1039 ;
char* tmpconstlift_1038 ;
char* tmpconstlift_1037 ;
char* tmpconstlift_1036 ;
char* tmpconstlift_1035 ;
char* tmpconstlift_1034 ;
char* tmpconstlift_1033 ;
char* tmpconstlift_1032 ;
char* tmpconstlift_1031 ;
char* tmpconstlift_1030 ;
char* tmpconstlift_1029 ;
char* tmpconstlift_1028 ;
char* tmpconstlift_1027 ;
char* tmpconstlift_1026 ;
char* tmpconstlift_1025 ;
char* tmpconstlift_1024 ;
char* tmpconstlift_1023 ;
char* tmpconstlift_1022 ;
char* tmpconstlift_1021 ;
char* tmpconstlift_1020 ;
char* tmpconstlift_1019 ;
char* tmpconstlift_1018 ;
char* tmpconstlift_1017 ;
char* tmpconstlift_1016 ;
char* tmpconstlift_1015 ;
char* tmpconstlift_1014 ;
char* tmpconstlift_1013 ;
char* tmpconstlift_1012 ;
char* tmpconstlift_1011 ;
char* tmpconstlift_1010 ;
char* tmpconstlift_1009 ;
char* tmpconstlift_1008 ;
char* tmpconstlift_1007 ;
char* tmpconstlift_1006 ;
char* tmpconstlift_1005 ;
char* tmpconstlift_1004 ;
char* tmpconstlift_1003 ;
char* tmpconstlift_1002 ;
char* tmpconstlift_1001 ;
char* tmpconstlift_1000 ;
char* tmpconstlift_999 ;
char* tmpconstlift_998 ;
char* tmpconstlift_997 ;
char* tmpconstlift_996 ;
char* tmpconstlift_995 ;
char* tmpconstlift_994 ;
char* tmpconstlift_993 ;
char* tmpconstlift_992 ;
char* tmpconstlift_991 ;
char* tmpconstlift_990 ;
char* tmpconstlift_989 ;
char* tmpconstlift_988 ;
char* tmpconstlift_987 ;
char* tmpconstlift_986 ;
char* tmpconstlift_985 ;
char* tmpconstlift_984 ;
char* tmpconstlift_983 ;
char* tmpconstlift_982 ;
char* tmpconstlift_981 ;
char* tmpconstlift_980 ;
char* tmpconstlift_979 ;
char* tmpconstlift_978 ;
char* tmpconstlift_977 ;
char* tmpconstlift_976 ;
char* tmpconstlift_975 ;
char* tmpconstlift_974 ;
char* tmpconstlift_973 ;
char* tmpconstlift_972 ;
char* tmpconstlift_971 ;
char* tmpconstlift_970 ;
void tmpsmp_1871(char x_962); // Iter prototype
void wsq_printer_1(struct tuptyp_2067 x_17); // Iter prototype
char Unix_fflush_15 ;
FILE* stdout_14 ;
void wsq_mergeMonotonic_2(struct Union2_1 x_22); // Iter prototype
struct tuptyp_2067** buf2_20 ;
struct tuptyp_2067** buf1_19 ;
void union2_3(struct Union2_1 arg_3725);
void stream_map_4(struct tuptyp_2067 x_76); // Iter prototype
void stream_map_5(struct tuptyp_2067 x_79); // Iter prototype
void wsq_randomSource_6(char __85); // Iter prototype
float* lastprice_83 ;
char** all_syms_82 ;
float t_81 ;
void socket_in_7(uint8_t* buf_88); // Iter prototype
void socket_in_raw_9(uint8_t* arg_3714);
void socket_in_raw_10(char __93); // Iter prototype
char spawn_socket_client_helper_90 ;
void socket_in_raw_11(char __114); // Iter prototype
char wsexit_111 ;
uint8_t* lenbuf_110 ;
char Unix_read_bytes_offset_109 ;
char Unix_puts_err_108 ;
char Unix_get_errno_107 ;
char shutdown_sockets_105 ;
char Unix_ewouldblock_104 ;
char poll_socket_client_ready_port_103 ;
int curstate_102 ;
uint8_t* databuf_101 ;
int msglen_100 ;
int wouldblock_99 ;
int datactr_98 ;
int sockfd_97 ;
int lenctr_96 ;
ws_bool_t connected_95 ;
DECLARE_WORKER(0, char, BASE)
DECLARE_WORKER(1, char, socket_in_raw_11)
DECLARE_WORKER(2, char, socket_in_raw_10)
DECLARE_WORKER(3, uint8_t*, socket_in_raw_9)
DECLARE_WORKER(4, uint8_t*, socket_in_7)
DECLARE_WORKER(5, char, wsq_randomSource_6)
DECLARE_WORKER(6, struct tuptyp_2067, stream_map_5)
DECLARE_WORKER(7, struct tuptyp_2067, stream_map_4)
DECLARE_WORKER(8, struct Union2_1, union2_3)
DECLARE_WORKER(9, struct Union2_1, wsq_mergeMonotonic_2)
DECLARE_WORKER(10, struct tuptyp_2067, wsq_printer_1)
DECLARE_WORKER(11, char, tmpsmp_1871)

void tmpsmp_1871(char x_962) {
  char ___VIRTQUEUE____963;
  GRAB_WRITEFIFO(BASE);
  printf("%s", tmpconstlift_1238);
  printf("%s", tmpconstlift_1237);
  printf("%s", tmpconstlift_1236);
  EMIT(((char)0), char, BASE);
  RELEASE_WRITEFIFO(BASE);
} 

void wsq_printer_1(struct tuptyp_2067 x_17) {
  char ___VIRTQUEUE____16;
  GRAB_WRITEFIFO(tmpsmp_1871);
  printf("%s", tmpconstlift_980);
  printf("%s", tmpconstlift_979);
  printf("%s", tmpconstlift_978);
  {
    float tmpsmp_1865 ;
     tmpsmp_1865 = (x_17.fld1);
    printf("%g", tmpsmp_1865);
  } 
  printf("%s", tmpconstlift_977);
  printf("%s", tmpconstlift_976);
  {
    char* tmpsmp_1863 ;
    {
      char* tmprc_2073 ;
       tmprc_2073 = (x_17.fld2);
       tmpsmp_1863 = tmprc_2073;
    } 
    printf("%s", tmpsmp_1863);
  } 
  printf("%s", tmpconstlift_975);
  printf("%s", tmpconstlift_974);
  {
    float tmpsmp_1861 ;
     tmpsmp_1861 = (x_17.fld3);
    printf("%g", tmpsmp_1861);
  } 
  printf("%s", tmpconstlift_973);
  printf("%s", tmpconstlift_972);
  {
    int tmpsmp_1859 ;
     tmpsmp_1859 = (x_17.fld4);
    printf("%d", tmpsmp_1859);
  } 
  printf("%s", tmpconstlift_971);
  printf("%s", tmpconstlift_970);
  {
    char ignored_valInEffect_1872 ;
    fflush(stdout_14);
     ignored_valInEffect_1872 = ((char)0);
  } 
  #ifdef WS_DISJOINT_HEAPS
  {
      // Copy & Release enqueued FIFO contents.
      int i;
      int pending = wsfifo_pending(& tmpsmp_1871_queue); 
      for(i=0; i < pending ; i++) { 
        void* ptr = wsfifo_recheck(& tmpsmp_1871_queue);
        // COPY IT
    
        wsfifo_release_one(& tmpsmp_1871_queue);
      }
  } 
  #endif
  RELEASE_WRITEFIFO(tmpsmp_1871);
} 

void wsq_mergeMonotonic_2(struct Union2_1 x_22) {
  char ___VIRTQUEUE____21;
  GRAB_WRITEFIFO(wsq_printer_1);
  {
    char ignored_valInEffect_1875 ;
    switch (x_22.tag) {
      case Left_1: {
        // This should be a reference... could maybe use a hack like #define
        struct tuptyp_2067 pattmp_1887 = x_22.payload.Left_1;
      {
        struct tuptyp_2067 a_65 ;
         a_65 = pattmp_1887;
        {
          struct tuptyp_2067* ls1_69 ;
           ls1_69 = buf1_19[0];
          {
            struct tuptyp_2067* ls2_68 ;
            struct tuptyp_2067* tmpcell_3743 = (struct tuptyp_2067*)CONSCELL(struct tuptyp_2067);
            SETCDR(tmpcell_3743, ((struct tuptyp_2067*)0), struct tuptyp_2067);
            SETCAR(tmpcell_3743, a_65, struct tuptyp_2067);
             ls2_68 = tmpcell_3743;
            {
              struct tuptyp_2067* ptr_73 ;
               ptr_73 = ls1_69;
              {
                struct tuptyp_2067* acc_74 ;
                 acc_74 = ((struct tuptyp_2067*)0);
                while (TRUE) {
                  ws_bool_t grosshack_3740 ;
                  {
                    ws_bool_t tmpsmp_1825 ;
                     tmpsmp_1825 = (ptr_73 == 0);
                    {
                      ws_bool_t tmpsmp_1827 ;
                       tmpsmp_1827 = !(tmpsmp_1825);
                       grosshack_3740 = tmpsmp_1827;
                    } 
                  } 
                  if (grosshack_3740) {
                    {
                      struct tuptyp_2067 tmpsmp_1831 ;
                       tmpsmp_1831 = CAR(ptr_73, struct tuptyp_2067);
                      {
                        struct tuptyp_2067* tmpsmp_1833 ;
                        struct tuptyp_2067* tmpcell_3739 = (struct tuptyp_2067*)CONSCELL(struct tuptyp_2067);
                        SETCDR(tmpcell_3739, acc_74, struct tuptyp_2067);
                        SETCAR(tmpcell_3739, tmpsmp_1831, struct tuptyp_2067);
                         tmpsmp_1833 = tmpcell_3739;
                        acc_74 = tmpsmp_1833;
                      } 
                    } 
                    {
                      struct tuptyp_2067* tmpsmp_1829 ;
                       tmpsmp_1829 = CDR(ptr_73, struct tuptyp_2067);
                      ptr_73 = tmpsmp_1829;
                    } 
                  } else break; 
                } 
                {
                  struct tuptyp_2067* tmpsmp_1837 ;
                   tmpsmp_1837 = acc_74;
                  {
                    struct tuptyp_2067* copy_70 ;
                     copy_70 = tmpsmp_1837;
                    {
                      struct tuptyp_2067* acc_71 ;
                       acc_71 = ls2_68;
                      while (TRUE) {
                        ws_bool_t grosshack_3742 ;
                        {
                          ws_bool_t tmpsmp_1839 ;
                           tmpsmp_1839 = (copy_70 == 0);
                          {
                            ws_bool_t tmpsmp_1841 ;
                             tmpsmp_1841 = !(tmpsmp_1839);
                             grosshack_3742 = tmpsmp_1841;
                          } 
                        } 
                        if (grosshack_3742) {
                          {
                            struct tuptyp_2067 tmpsmp_1845 ;
                             tmpsmp_1845 = CAR(copy_70, struct tuptyp_2067);
                            {
                              struct tuptyp_2067* tmpsmp_1847 ;
                              struct tuptyp_2067* tmpcell_3741 = (struct tuptyp_2067*)CONSCELL(struct tuptyp_2067);
                              SETCDR(tmpcell_3741, acc_71, struct tuptyp_2067);
                              SETCAR(tmpcell_3741, tmpsmp_1845, struct tuptyp_2067);
                               tmpsmp_1847 = tmpcell_3741;
                              acc_71 = tmpsmp_1847;
                            } 
                          } 
                          {
                            struct tuptyp_2067* tmpsmp_1843 ;
                             tmpsmp_1843 = CDR(copy_70, struct tuptyp_2067);
                            copy_70 = tmpsmp_1843;
                          } 
                        } else break; 
                      } 
                      {
                        struct tuptyp_2067* tmpsmp_1851 ;
                         tmpsmp_1851 = acc_71;
                        buf1_19[0] = tmpsmp_1851;
                         ignored_valInEffect_1875 = ((char)0);
                      } 
                    } 
                  } 
                } 
              } 
            } 
          } 
        } 
      } 
        }  break;
      case Right_1: {
        // This should be a reference... could maybe use a hack like #define
        struct tuptyp_2067 pattmp_1886 = x_22.payload.Right_1;
      {
        struct tuptyp_2067 b_55 ;
         b_55 = pattmp_1886;
        {
          struct tuptyp_2067* ls1_59 ;
           ls1_59 = buf2_20[0];
          {
            struct tuptyp_2067* ls2_58 ;
            struct tuptyp_2067* tmpcell_3738 = (struct tuptyp_2067*)CONSCELL(struct tuptyp_2067);
            SETCDR(tmpcell_3738, ((struct tuptyp_2067*)0), struct tuptyp_2067);
            SETCAR(tmpcell_3738, b_55, struct tuptyp_2067);
             ls2_58 = tmpcell_3738;
            {
              struct tuptyp_2067* ptr_63 ;
               ptr_63 = ls1_59;
              {
                struct tuptyp_2067* acc_64 ;
                 acc_64 = ((struct tuptyp_2067*)0);
                while (TRUE) {
                  ws_bool_t grosshack_3735 ;
                  {
                    ws_bool_t tmpsmp_1795 ;
                     tmpsmp_1795 = (ptr_63 == 0);
                    {
                      ws_bool_t tmpsmp_1797 ;
                       tmpsmp_1797 = !(tmpsmp_1795);
                       grosshack_3735 = tmpsmp_1797;
                    } 
                  } 
                  if (grosshack_3735) {
                    {
                      struct tuptyp_2067 tmpsmp_1801 ;
                       tmpsmp_1801 = CAR(ptr_63, struct tuptyp_2067);
                      {
                        struct tuptyp_2067* tmpsmp_1803 ;
                        struct tuptyp_2067* tmpcell_3734 = (struct tuptyp_2067*)CONSCELL(struct tuptyp_2067);
                        SETCDR(tmpcell_3734, acc_64, struct tuptyp_2067);
                        SETCAR(tmpcell_3734, tmpsmp_1801, struct tuptyp_2067);
                         tmpsmp_1803 = tmpcell_3734;
                        acc_64 = tmpsmp_1803;
                      } 
                    } 
                    {
                      struct tuptyp_2067* tmpsmp_1799 ;
                       tmpsmp_1799 = CDR(ptr_63, struct tuptyp_2067);
                      ptr_63 = tmpsmp_1799;
                    } 
                  } else break; 
                } 
                {
                  struct tuptyp_2067* tmpsmp_1807 ;
                   tmpsmp_1807 = acc_64;
                  {
                    struct tuptyp_2067* copy_60 ;
                     copy_60 = tmpsmp_1807;
                    {
                      struct tuptyp_2067* acc_61 ;
                       acc_61 = ls2_58;
                      while (TRUE) {
                        ws_bool_t grosshack_3737 ;
                        {
                          ws_bool_t tmpsmp_1809 ;
                           tmpsmp_1809 = (copy_60 == 0);
                          {
                            ws_bool_t tmpsmp_1811 ;
                             tmpsmp_1811 = !(tmpsmp_1809);
                             grosshack_3737 = tmpsmp_1811;
                          } 
                        } 
                        if (grosshack_3737) {
                          {
                            struct tuptyp_2067 tmpsmp_1815 ;
                             tmpsmp_1815 = CAR(copy_60, struct tuptyp_2067);
                            {
                              struct tuptyp_2067* tmpsmp_1817 ;
                              struct tuptyp_2067* tmpcell_3736 = (struct tuptyp_2067*)CONSCELL(struct tuptyp_2067);
                              SETCDR(tmpcell_3736, acc_61, struct tuptyp_2067);
                              SETCAR(tmpcell_3736, tmpsmp_1815, struct tuptyp_2067);
                               tmpsmp_1817 = tmpcell_3736;
                              acc_61 = tmpsmp_1817;
                            } 
                          } 
                          {
                            struct tuptyp_2067* tmpsmp_1813 ;
                             tmpsmp_1813 = CDR(copy_60, struct tuptyp_2067);
                            copy_60 = tmpsmp_1813;
                          } 
                        } else break; 
                      } 
                      {
                        struct tuptyp_2067* tmpsmp_1821 ;
                         tmpsmp_1821 = acc_61;
                        buf2_20[0] = tmpsmp_1821;
                         ignored_valInEffect_1875 = ((char)0);
                      } 
                    } 
                  } 
                } 
              } 
            } 
          } 
        } 
      } 
        }  break;
    } 
  } 
  while (TRUE) {
    ws_bool_t grosshack_3746 ;
    {
      struct tuptyp_2067* ls_52 ;
      {
        struct tuptyp_2067* tmprc_2112 ;
         tmprc_2112 = buf1_19[0];
         ls_52 = tmprc_2112;
      } 
      {
        int count_53 ;
        {
          int tmprc_2115 ;
           tmprc_2115 = 0;
           count_53 = tmprc_2115;
        } 
        {
          struct tuptyp_2067* ptr_54 ;
          {
            struct tuptyp_2067* tmprc_2118 ;
             tmprc_2118 = ls_52;
             ptr_54 = tmprc_2118;
          } 
          while (TRUE) {
            ws_bool_t grosshack_3747 ;
            {
              struct tuptyp_2067* lsptr1_1293 ;
              {
                struct tuptyp_2067* tmprc_2204 ;
                 tmprc_2204 = ptr_54;
                 lsptr1_1293 = tmprc_2204;
              } 
              {
                struct tuptyp_2067* lsptr2_1294 ;
                {
                  struct tuptyp_2067* tmprc_2207 ;
                   tmprc_2207 = ((struct tuptyp_2067*)0);
                   lsptr2_1294 = tmprc_2207;
                } 
                {
                  ws_bool_t stop_1297 ;
                  {
                    ws_bool_t tmprc_2210 ;
                     tmprc_2210 = FALSE;
                     stop_1297 = tmprc_2210;
                  } 
                  {
                    ws_bool_t result_1298 ;
                    {
                      ws_bool_t tmprc_2213 ;
                       tmprc_2213 = FALSE;
                       result_1298 = tmprc_2213;
                    } 
                    while (TRUE) {
                      ws_bool_t grosshack_3749 ;
                      {
                        ws_bool_t tmpsmp_1575 ;
                         tmpsmp_1575 = !(stop_1297);
                         grosshack_3749 = tmpsmp_1575;
                      } 
                      if (grosshack_3749) {
                        {
                          ws_bool_t tmpsmp_1577 ;
                           tmpsmp_1577 = (lsptr1_1293 == 0);
                          {
                            char tmpsmp_1651 ;
                            if (tmpsmp_1577) {
                              {
                                ws_bool_t tmpsmp_1579 ;
                                 tmpsmp_1579 = (lsptr2_1294 == 0);
                                {
                                  char ignored_valInEffect_1874 ;
                                  if (tmpsmp_1579) {
                                    result_1298 = TRUE;
                                     ignored_valInEffect_1874 = ((char)0);
                                  } else {
                                     ignored_valInEffect_1874 = ((char)0);
                                  }
                                } 
                              } 
                              stop_1297 = TRUE;
                               tmpsmp_1651 = ((char)0);
                            } else {
                              {
                                ws_bool_t tmpsmp_1585 ;
                                 tmpsmp_1585 = (lsptr2_1294 == 0);
                                {
                                  char tmpsmp_1649 ;
                                  if (tmpsmp_1585) {
                                    stop_1297 = TRUE;
                                     tmpsmp_1649 = ((char)0);
                                  } else {
                                    {
                                      struct tuptyp_2067 lsel1_1295 ;
                                      {
                                        struct tuptyp_2067 tmprc_2220 ;
                                         tmprc_2220 = CAR(lsptr1_1293, struct tuptyp_2067);
                                         lsel1_1295 = tmprc_2220;
                                      } 
                                      {
                                        struct tuptyp_2067 lsel2_1296 ;
                                        {
                                          struct tuptyp_2067 tmprc_2223 ;
                                           tmprc_2223 = CAR(lsptr2_1294, struct tuptyp_2067);
                                           lsel2_1296 = tmprc_2223;
                                        } 
                                        {
                                          float tmpsmp_1589 ;
                                           tmpsmp_1589 = (lsel1_1295.fld1);
                                          {
                                            float tmpsmp_1591 ;
                                             tmpsmp_1591 = (lsel2_1296.fld1);
                                            {
                                              ws_bool_t tmpsmp_1593 ;
                                               tmpsmp_1593 = (tmpsmp_1589 == tmpsmp_1591);
                                              {
                                                ws_bool_t tmpsmp_1637 ;
                                                if (tmpsmp_1593) {
                                                  {
                                                    char* arr1_1299 ;
                                                    {
                                                      char* tmprc_2230 ;
                                                       tmprc_2230 = (lsel1_1295.fld2);
                                                       arr1_1299 = tmprc_2230;
                                                    } 
                                                    {
                                                      char* arr2_1300 ;
                                                      {
                                                        char* tmprc_2233 ;
                                                         tmprc_2233 = (lsel2_1296.fld2);
                                                         arr2_1300 = tmprc_2233;
                                                      } 
                                                      {
                                                        int tmpsmp_1595 ;
                                                         tmpsmp_1595 = ARRLEN(arr1_1299);
                                                        {
                                                          int tmpsmp_1597 ;
                                                           tmpsmp_1597 = ARRLEN(arr2_1300);
                                                          {
                                                            ws_bool_t tmpsmp_1599 ;
                                                             tmpsmp_1599 = (tmpsmp_1595 == tmpsmp_1597);
                                                            {
                                                              ws_bool_t tmpsmp_1619 ;
                                                              if (tmpsmp_1599) {
                                                                {
                                                                  int i_1303 ;
                                                                  {
                                                                    int tmprc_2240 ;
                                                                     tmprc_2240 = 0;
                                                                     i_1303 = tmprc_2240;
                                                                  } 
                                                                  {
                                                                    ws_bool_t stop_1304 ;
                                                                    {
                                                                      ws_bool_t tmprc_2243 ;
                                                                       tmprc_2243 = FALSE;
                                                                       stop_1304 = tmprc_2243;
                                                                    } 
                                                                    {
                                                                      int len_1305 ;
                                                                       len_1305 = ARRLEN(arr1_1299);
                                                                      while (TRUE) {
                                                                        ws_bool_t grosshack_3748 ;
                                                                        {
                                                                          ws_bool_t tmpsmp_1601 ;
                                                                           tmpsmp_1601 = (i_1303 < len_1305);
                                                                          {
                                                                            ws_bool_t tmpsmp_1605 ;
                                                                            if (tmpsmp_1601) {
                                                                              {
                                                                                ws_bool_t tmpsmp_1603 ;
                                                                                 tmpsmp_1603 = !(stop_1304);
                                                                                 tmpsmp_1605 = tmpsmp_1603;
                                                                              } 
                                                                            } else {
                                                                               tmpsmp_1605 = FALSE;
                                                                            }
                                                                             grosshack_3748 = tmpsmp_1605;
                                                                          } 
                                                                        } 
                                                                        if (grosshack_3748) {
                                                                          {
                                                                            char arrel1_1301 ;
                                                                             arrel1_1301 = arr1_1299[i_1303];
                                                                            {
                                                                              char arrel2_1302 ;
                                                                               arrel2_1302 = arr2_1300[i_1303];
                                                                              {
                                                                                ws_bool_t tmpsmp_1607 ;
                                                                                 tmpsmp_1607 = (arrel1_1301 == arrel2_1302);
                                                                                {
                                                                                  char tmpsmp_1615 ;
                                                                                  if (tmpsmp_1607) {
                                                                                    {
                                                                                      int tmpsmp_1609 ;
                                                                                       tmpsmp_1609 = (i_1303 + 1);
                                                                                      i_1303 = tmpsmp_1609;
                                                                                       tmpsmp_1615 = ((char)0);
                                                                                    } 
                                                                                  } else {
                                                                                    stop_1304 = TRUE;
                                                                                     tmpsmp_1615 = ((char)0);
                                                                                  }
                                                                                } 
                                                                              } 
                                                                            } 
                                                                          } 
                                                                        } else break; 
                                                                      } 
                                                                      {
                                                                        ws_bool_t tmpsmp_1617 ;
                                                                         tmpsmp_1617 = !(stop_1304);
                                                                         tmpsmp_1619 = tmpsmp_1617;
                                                                      } 
                                                                    } 
                                                                  } 
                                                                } 
                                                              } else {
                                                                 tmpsmp_1619 = FALSE;
                                                              }
                                                              {
                                                                ws_bool_t tmpsmp_1635 ;
                                                                if (tmpsmp_1619) {
                                                                  {
                                                                    float tmpsmp_1621 ;
                                                                     tmpsmp_1621 = (lsel1_1295.fld3);
                                                                    {
                                                                      float tmpsmp_1623 ;
                                                                       tmpsmp_1623 = (lsel2_1296.fld3);
                                                                      {
                                                                        ws_bool_t tmpsmp_1625 ;
                                                                         tmpsmp_1625 = (tmpsmp_1621 == tmpsmp_1623);
                                                                        {
                                                                          ws_bool_t tmpsmp_1633 ;
                                                                          if (tmpsmp_1625) {
                                                                            {
                                                                              int tmpsmp_1627 ;
                                                                               tmpsmp_1627 = (lsel1_1295.fld4);
                                                                              {
                                                                                int tmpsmp_1629 ;
                                                                                 tmpsmp_1629 = (lsel2_1296.fld4);
                                                                                {
                                                                                  ws_bool_t tmpsmp_1631 ;
                                                                                   tmpsmp_1631 = (tmpsmp_1627 == tmpsmp_1629);
                                                                                   tmpsmp_1633 = tmpsmp_1631;
                                                                                } 
                                                                              } 
                                                                            } 
                                                                          } else {
                                                                             tmpsmp_1633 = FALSE;
                                                                          }
                                                                           tmpsmp_1635 = tmpsmp_1633;
                                                                        } 
                                                                      } 
                                                                    } 
                                                                  } 
                                                                } else {
                                                                   tmpsmp_1635 = FALSE;
                                                                }
                                                                 tmpsmp_1637 = tmpsmp_1635;
                                                              } 
                                                            } 
                                                          } 
                                                        } 
                                                      } 
                                                    } 
                                                  } 
                                                } else {
                                                   tmpsmp_1637 = FALSE;
                                                }
                                                {
                                                  char tmpsmp_1647 ;
                                                  if (tmpsmp_1637) {
                                                    {
                                                      struct tuptyp_2067* tmpsmp_1641 ;
                                                      {
                                                        struct tuptyp_2067* tmprc_2265 ;
                                                         tmprc_2265 = CDR(lsptr1_1293, struct tuptyp_2067);
                                                         tmpsmp_1641 = tmprc_2265;
                                                      } 
                                                      {
                                                        struct tuptyp_2067* settmp_2264 ;
                                                         settmp_2264 = lsptr1_1293;
                                                        lsptr1_1293 = tmpsmp_1641;
                                                      } 
                                                    } 
                                                    {
                                                      struct tuptyp_2067* tmpsmp_1639 ;
                                                      {
                                                        struct tuptyp_2067* tmprc_2261 ;
                                                         tmprc_2261 = CDR(lsptr2_1294, struct tuptyp_2067);
                                                         tmpsmp_1639 = tmprc_2261;
                                                      } 
                                                      {
                                                        struct tuptyp_2067* settmp_2263 ;
                                                         settmp_2263 = lsptr2_1294;
                                                        lsptr2_1294 = tmpsmp_1639;
                                                      } 
                                                       tmpsmp_1647 = ((char)0);
                                                    } 
                                                  } else {
                                                    stop_1297 = TRUE;
                                                     tmpsmp_1647 = ((char)0);
                                                  }
                                                   tmpsmp_1649 = tmpsmp_1647;
                                                } 
                                              } 
                                            } 
                                          } 
                                        } 
                                      } 
                                    } 
                                  }
                                   tmpsmp_1651 = tmpsmp_1649;
                                } 
                              } 
                            }
                          } 
                        } 
                      } else break; 
                    } 
                    {
                      ws_bool_t tmpsmp_1653 ;
                       tmpsmp_1653 = result_1298;
                      {
                        ws_bool_t tmpsmp_1655 ;
                         tmpsmp_1655 = !(tmpsmp_1653);
                         grosshack_3747 = tmpsmp_1655;
                      } 
                    } 
                  } 
                } 
              } 
            } 
            if (grosshack_3747) {
              {
                struct tuptyp_2067* tmpsmp_1659 ;
                {
                  struct tuptyp_2067* tmprc_2202 ;
                   tmprc_2202 = CDR(ptr_54, struct tuptyp_2067);
                   tmpsmp_1659 = tmprc_2202;
                } 
                {
                  struct tuptyp_2067* settmp_2201 ;
                   settmp_2201 = ptr_54;
                  ptr_54 = tmpsmp_1659;
                } 
              } 
              {
                int tmpsmp_1657 ;
                 tmpsmp_1657 = (count_53 + 1);
                count_53 = tmpsmp_1657;
              } 
            } else break; 
          } 
          {
            int tmpsmp_1663 ;
             tmpsmp_1663 = count_53;
            {
              ws_bool_t tmpsmp_1665 ;
               tmpsmp_1665 = (tmpsmp_1663 > 0);
              {
                ws_bool_t tmpsmp_1761 ;
                if (tmpsmp_1665) {
                  {
                    struct tuptyp_2067* ls_48 ;
                    {
                      struct tuptyp_2067* tmprc_2124 ;
                       tmprc_2124 = buf2_20[0];
                       ls_48 = tmprc_2124;
                    } 
                    {
                      int count_49 ;
                      {
                        int tmprc_2127 ;
                         tmprc_2127 = 0;
                         count_49 = tmprc_2127;
                      } 
                      {
                        struct tuptyp_2067* ptr_50 ;
                        {
                          struct tuptyp_2067* tmprc_2130 ;
                           tmprc_2130 = ls_48;
                           ptr_50 = tmprc_2130;
                        } 
                        while (TRUE) {
                          ws_bool_t grosshack_3750 ;
                          {
                            struct tuptyp_2067* lsptr1_1280 ;
                            {
                              struct tuptyp_2067* tmprc_2138 ;
                               tmprc_2138 = ptr_50;
                               lsptr1_1280 = tmprc_2138;
                            } 
                            {
                              struct tuptyp_2067* lsptr2_1281 ;
                              {
                                struct tuptyp_2067* tmprc_2141 ;
                                 tmprc_2141 = ((struct tuptyp_2067*)0);
                                 lsptr2_1281 = tmprc_2141;
                              } 
                              {
                                ws_bool_t stop_1284 ;
                                {
                                  ws_bool_t tmprc_2144 ;
                                   tmprc_2144 = FALSE;
                                   stop_1284 = tmprc_2144;
                                } 
                                {
                                  ws_bool_t result_1285 ;
                                  {
                                    ws_bool_t tmprc_2147 ;
                                     tmprc_2147 = FALSE;
                                     result_1285 = tmprc_2147;
                                  } 
                                  while (TRUE) {
                                    ws_bool_t grosshack_3752 ;
                                    {
                                      ws_bool_t tmpsmp_1667 ;
                                       tmpsmp_1667 = !(stop_1284);
                                       grosshack_3752 = tmpsmp_1667;
                                    } 
                                    if (grosshack_3752) {
                                      {
                                        ws_bool_t tmpsmp_1669 ;
                                         tmpsmp_1669 = (lsptr1_1280 == 0);
                                        {
                                          char tmpsmp_1743 ;
                                          if (tmpsmp_1669) {
                                            {
                                              ws_bool_t tmpsmp_1671 ;
                                               tmpsmp_1671 = (lsptr2_1281 == 0);
                                              {
                                                char ignored_valInEffect_1873 ;
                                                if (tmpsmp_1671) {
                                                  result_1285 = TRUE;
                                                   ignored_valInEffect_1873 = ((char)0);
                                                } else {
                                                   ignored_valInEffect_1873 = ((char)0);
                                                }
                                              } 
                                            } 
                                            stop_1284 = TRUE;
                                             tmpsmp_1743 = ((char)0);
                                          } else {
                                            {
                                              ws_bool_t tmpsmp_1677 ;
                                               tmpsmp_1677 = (lsptr2_1281 == 0);
                                              {
                                                char tmpsmp_1741 ;
                                                if (tmpsmp_1677) {
                                                  stop_1284 = TRUE;
                                                   tmpsmp_1741 = ((char)0);
                                                } else {
                                                  {
                                                    struct tuptyp_2067 lsel1_1282 ;
                                                    {
                                                      struct tuptyp_2067 tmprc_2154 ;
                                                       tmprc_2154 = CAR(lsptr1_1280, struct tuptyp_2067);
                                                       lsel1_1282 = tmprc_2154;
                                                    } 
                                                    {
                                                      struct tuptyp_2067 lsel2_1283 ;
                                                      {
                                                        struct tuptyp_2067 tmprc_2157 ;
                                                         tmprc_2157 = CAR(lsptr2_1281, struct tuptyp_2067);
                                                         lsel2_1283 = tmprc_2157;
                                                      } 
                                                      {
                                                        float tmpsmp_1681 ;
                                                         tmpsmp_1681 = (lsel1_1282.fld1);
                                                        {
                                                          float tmpsmp_1683 ;
                                                           tmpsmp_1683 = (lsel2_1283.fld1);
                                                          {
                                                            ws_bool_t tmpsmp_1685 ;
                                                             tmpsmp_1685 = (tmpsmp_1681 == tmpsmp_1683);
                                                            {
                                                              ws_bool_t tmpsmp_1729 ;
                                                              if (tmpsmp_1685) {
                                                                {
                                                                  char* arr1_1286 ;
                                                                  {
                                                                    char* tmprc_2164 ;
                                                                     tmprc_2164 = (lsel1_1282.fld2);
                                                                     arr1_1286 = tmprc_2164;
                                                                  } 
                                                                  {
                                                                    char* arr2_1287 ;
                                                                    {
                                                                      char* tmprc_2167 ;
                                                                       tmprc_2167 = (lsel2_1283.fld2);
                                                                       arr2_1287 = tmprc_2167;
                                                                    } 
                                                                    {
                                                                      int tmpsmp_1687 ;
                                                                       tmpsmp_1687 = ARRLEN(arr1_1286);
                                                                      {
                                                                        int tmpsmp_1689 ;
                                                                         tmpsmp_1689 = ARRLEN(arr2_1287);
                                                                        {
                                                                          ws_bool_t tmpsmp_1691 ;
                                                                           tmpsmp_1691 = (tmpsmp_1687 == tmpsmp_1689);
                                                                          {
                                                                            ws_bool_t tmpsmp_1711 ;
                                                                            if (tmpsmp_1691) {
                                                                              {
                                                                                int i_1290 ;
                                                                                {
                                                                                  int tmprc_2174 ;
                                                                                   tmprc_2174 = 0;
                                                                                   i_1290 = tmprc_2174;
                                                                                } 
                                                                                {
                                                                                  ws_bool_t stop_1291 ;
                                                                                  {
                                                                                    ws_bool_t tmprc_2177 ;
                                                                                     tmprc_2177 = FALSE;
                                                                                     stop_1291 = tmprc_2177;
                                                                                  } 
                                                                                  {
                                                                                    int len_1292 ;
                                                                                     len_1292 = ARRLEN(arr1_1286);
                                                                                    while (TRUE) {
                                                                                      ws_bool_t grosshack_3751 ;
                                                                                      {
                                                                                        ws_bool_t tmpsmp_1693 ;
                                                                                         tmpsmp_1693 = (i_1290 < len_1292);
                                                                                        {
                                                                                          ws_bool_t tmpsmp_1697 ;
                                                                                          if (tmpsmp_1693) {
                                                                                            {
                                                                                              ws_bool_t tmpsmp_1695 ;
                                                                                               tmpsmp_1695 = !(stop_1291);
                                                                                               tmpsmp_1697 = tmpsmp_1695;
                                                                                            } 
                                                                                          } else {
                                                                                             tmpsmp_1697 = FALSE;
                                                                                          }
                                                                                           grosshack_3751 = tmpsmp_1697;
                                                                                        } 
                                                                                      } 
                                                                                      if (grosshack_3751) {
                                                                                        {
                                                                                          char arrel1_1288 ;
                                                                                           arrel1_1288 = arr1_1286[i_1290];
                                                                                          {
                                                                                            char arrel2_1289 ;
                                                                                             arrel2_1289 = arr2_1287[i_1290];
                                                                                            {
                                                                                              ws_bool_t tmpsmp_1699 ;
                                                                                               tmpsmp_1699 = (arrel1_1288 == arrel2_1289);
                                                                                              {
                                                                                                char tmpsmp_1707 ;
                                                                                                if (tmpsmp_1699) {
                                                                                                  {
                                                                                                    int tmpsmp_1701 ;
                                                                                                     tmpsmp_1701 = (i_1290 + 1);
                                                                                                    i_1290 = tmpsmp_1701;
                                                                                                     tmpsmp_1707 = ((char)0);
                                                                                                  } 
                                                                                                } else {
                                                                                                  stop_1291 = TRUE;
                                                                                                   tmpsmp_1707 = ((char)0);
                                                                                                }
                                                                                              } 
                                                                                            } 
                                                                                          } 
                                                                                        } 
                                                                                      } else break; 
                                                                                    } 
                                                                                    {
                                                                                      ws_bool_t tmpsmp_1709 ;
                                                                                       tmpsmp_1709 = !(stop_1291);
                                                                                       tmpsmp_1711 = tmpsmp_1709;
                                                                                    } 
                                                                                  } 
                                                                                } 
                                                                              } 
                                                                            } else {
                                                                               tmpsmp_1711 = FALSE;
                                                                            }
                                                                            {
                                                                              ws_bool_t tmpsmp_1727 ;
                                                                              if (tmpsmp_1711) {
                                                                                {
                                                                                  float tmpsmp_1713 ;
                                                                                   tmpsmp_1713 = (lsel1_1282.fld3);
                                                                                  {
                                                                                    float tmpsmp_1715 ;
                                                                                     tmpsmp_1715 = (lsel2_1283.fld3);
                                                                                    {
                                                                                      ws_bool_t tmpsmp_1717 ;
                                                                                       tmpsmp_1717 = (tmpsmp_1713 == tmpsmp_1715);
                                                                                      {
                                                                                        ws_bool_t tmpsmp_1725 ;
                                                                                        if (tmpsmp_1717) {
                                                                                          {
                                                                                            int tmpsmp_1719 ;
                                                                                             tmpsmp_1719 = (lsel1_1282.fld4);
                                                                                            {
                                                                                              int tmpsmp_1721 ;
                                                                                               tmpsmp_1721 = (lsel2_1283.fld4);
                                                                                              {
                                                                                                ws_bool_t tmpsmp_1723 ;
                                                                                                 tmpsmp_1723 = (tmpsmp_1719 == tmpsmp_1721);
                                                                                                 tmpsmp_1725 = tmpsmp_1723;
                                                                                              } 
                                                                                            } 
                                                                                          } 
                                                                                        } else {
                                                                                           tmpsmp_1725 = FALSE;
                                                                                        }
                                                                                         tmpsmp_1727 = tmpsmp_1725;
                                                                                      } 
                                                                                    } 
                                                                                  } 
                                                                                } 
                                                                              } else {
                                                                                 tmpsmp_1727 = FALSE;
                                                                              }
                                                                               tmpsmp_1729 = tmpsmp_1727;
                                                                            } 
                                                                          } 
                                                                        } 
                                                                      } 
                                                                    } 
                                                                  } 
                                                                } 
                                                              } else {
                                                                 tmpsmp_1729 = FALSE;
                                                              }
                                                              {
                                                                char tmpsmp_1739 ;
                                                                if (tmpsmp_1729) {
                                                                  {
                                                                    struct tuptyp_2067* tmpsmp_1733 ;
                                                                    {
                                                                      struct tuptyp_2067* tmprc_2199 ;
                                                                       tmprc_2199 = CDR(lsptr1_1280, struct tuptyp_2067);
                                                                       tmpsmp_1733 = tmprc_2199;
                                                                    } 
                                                                    {
                                                                      struct tuptyp_2067* settmp_2198 ;
                                                                       settmp_2198 = lsptr1_1280;
                                                                      lsptr1_1280 = tmpsmp_1733;
                                                                    } 
                                                                  } 
                                                                  {
                                                                    struct tuptyp_2067* tmpsmp_1731 ;
                                                                    {
                                                                      struct tuptyp_2067* tmprc_2195 ;
                                                                       tmprc_2195 = CDR(lsptr2_1281, struct tuptyp_2067);
                                                                       tmpsmp_1731 = tmprc_2195;
                                                                    } 
                                                                    {
                                                                      struct tuptyp_2067* settmp_2197 ;
                                                                       settmp_2197 = lsptr2_1281;
                                                                      lsptr2_1281 = tmpsmp_1731;
                                                                    } 
                                                                     tmpsmp_1739 = ((char)0);
                                                                  } 
                                                                } else {
                                                                  stop_1284 = TRUE;
                                                                   tmpsmp_1739 = ((char)0);
                                                                }
                                                                 tmpsmp_1741 = tmpsmp_1739;
                                                              } 
                                                            } 
                                                          } 
                                                        } 
                                                      } 
                                                    } 
                                                  } 
                                                }
                                                 tmpsmp_1743 = tmpsmp_1741;
                                              } 
                                            } 
                                          }
                                        } 
                                      } 
                                    } else break; 
                                  } 
                                  {
                                    ws_bool_t tmpsmp_1745 ;
                                     tmpsmp_1745 = result_1285;
                                    {
                                      ws_bool_t tmpsmp_1747 ;
                                       tmpsmp_1747 = !(tmpsmp_1745);
                                       grosshack_3750 = tmpsmp_1747;
                                    } 
                                  } 
                                } 
                              } 
                            } 
                          } 
                          if (grosshack_3750) {
                            {
                              struct tuptyp_2067* tmpsmp_1751 ;
                              {
                                struct tuptyp_2067* tmprc_2136 ;
                                 tmprc_2136 = CDR(ptr_50, struct tuptyp_2067);
                                 tmpsmp_1751 = tmprc_2136;
                              } 
                              {
                                struct tuptyp_2067* settmp_2135 ;
                                 settmp_2135 = ptr_50;
                                ptr_50 = tmpsmp_1751;
                              } 
                            } 
                            {
                              int tmpsmp_1749 ;
                               tmpsmp_1749 = (count_49 + 1);
                              count_49 = tmpsmp_1749;
                            } 
                          } else break; 
                        } 
                        {
                          int tmpsmp_1755 ;
                           tmpsmp_1755 = count_49;
                          {
                            ws_bool_t tmpsmp_1757 ;
                             tmpsmp_1757 = (tmpsmp_1755 > 0);
                            {
                              ws_bool_t tmpsmp_1759 ;
                              if (tmpsmp_1757) {
                                 tmpsmp_1759 = TRUE;
                              } else {
                                 tmpsmp_1759 = FALSE;
                              }
                               tmpsmp_1761 = tmpsmp_1759;
                            } 
                          } 
                        } 
                      } 
                    } 
                  } 
                } else {
                   tmpsmp_1761 = FALSE;
                }
                 grosshack_3746 = tmpsmp_1761;
              } 
            } 
          } 
        } 
      } 
    } 
    if (grosshack_3746) {
      {
        struct tuptyp_2067* ls_44 ;
        {
          struct tuptyp_2067* tmprc_2110 ;
           tmprc_2110 = buf1_19[0];
           ls_44 = tmprc_2110;
        } 
        {
          int i_45 ;
          {
            int tmprc_2109 ;
             tmprc_2109 = 0;
             i_45 = tmprc_2109;
          } 
          {
            struct tuptyp_2067* ptr_46 ;
            {
              struct tuptyp_2067* tmprc_2108 ;
               tmprc_2108 = ls_44;
               ptr_46 = tmprc_2108;
            } 
            while (TRUE) {
              ws_bool_t grosshack_3745 ;
              {
                ws_bool_t tmpsmp_1763 ;
                 tmpsmp_1763 = (i_45 < 0);
                 grosshack_3745 = tmpsmp_1763;
              } 
              if (grosshack_3745) {
                {
                  int tmpsmp_1767 ;
                   tmpsmp_1767 = (i_45 + 1);
                  i_45 = tmpsmp_1767;
                } 
                {
                  struct tuptyp_2067* tmpsmp_1765 ;
                  {
                    struct tuptyp_2067* tmprc_2106 ;
                     tmprc_2106 = CDR(ptr_46, struct tuptyp_2067);
                     tmpsmp_1765 = tmprc_2106;
                  } 
                  {
                    struct tuptyp_2067* settmp_2105 ;
                     settmp_2105 = ptr_46;
                    ptr_46 = tmpsmp_1765;
                  } 
                } 
              } else break; 
            } 
            {
              struct tuptyp_2067 a_23 ;
              {
                struct tuptyp_2067 tmprc_2104 ;
                 tmprc_2104 = CAR(ptr_46, struct tuptyp_2067);
                 a_23 = tmprc_2104;
              } 
              {
                struct tuptyp_2067* ls_38 ;
                {
                  struct tuptyp_2067* tmprc_2103 ;
                   tmprc_2103 = buf2_20[0];
                   ls_38 = tmprc_2103;
                } 
                {
                  int i_39 ;
                  {
                    int tmprc_2102 ;
                     tmprc_2102 = 0;
                     i_39 = tmprc_2102;
                  } 
                  {
                    struct tuptyp_2067* ptr_40 ;
                    {
                      struct tuptyp_2067* tmprc_2101 ;
                       tmprc_2101 = ls_38;
                       ptr_40 = tmprc_2101;
                    } 
                    while (TRUE) {
                      ws_bool_t grosshack_3744 ;
                      {
                        ws_bool_t tmpsmp_1771 ;
                         tmpsmp_1771 = (i_39 < 0);
                         grosshack_3744 = tmpsmp_1771;
                      } 
                      if (grosshack_3744) {
                        {
                          int tmpsmp_1775 ;
                           tmpsmp_1775 = (i_39 + 1);
                          i_39 = tmpsmp_1775;
                        } 
                        {
                          struct tuptyp_2067* tmpsmp_1773 ;
                          {
                            struct tuptyp_2067* tmprc_2099 ;
                             tmprc_2099 = CDR(ptr_40, struct tuptyp_2067);
                             tmpsmp_1773 = tmprc_2099;
                          } 
                          {
                            struct tuptyp_2067* settmp_2098 ;
                             settmp_2098 = ptr_40;
                            ptr_40 = tmpsmp_1773;
                          } 
                        } 
                      } else break; 
                    } 
                    {
                      struct tuptyp_2067 b_24 ;
                      {
                        struct tuptyp_2067 tmprc_2097 ;
                         tmprc_2097 = CAR(ptr_40, struct tuptyp_2067);
                         b_24 = tmprc_2097;
                      } 
                      {
                        float tmpsmp_1779 ;
                         tmpsmp_1779 = (a_23.fld3);
                        {
                          float tmpsmp_1781 ;
                           tmpsmp_1781 = (b_24.fld3);
                          {
                            ws_bool_t tmpsmp_1783 ;
                             tmpsmp_1783 = (tmpsmp_1779 <= tmpsmp_1781);
                            {
                              char tmpsmp_1793 ;
                              if (tmpsmp_1783) {
                                {
                                  struct tuptyp_2067* a_32 ;
                                  {
                                    struct tuptyp_2067* tmprc_2096 ;
                                     tmprc_2096 = buf1_19[0];
                                     a_32 = tmprc_2096;
                                  } 
                                  {
                                    struct tuptyp_2067 x_30 ;
                                    {
                                      struct tuptyp_2067 tmprc_2095 ;
                                       tmprc_2095 = CAR(a_32, struct tuptyp_2067);
                                       x_30 = tmprc_2095;
                                    } 
                                    {
                                      struct tuptyp_2067* a_31 ;
                                      {
                                        struct tuptyp_2067* tmprc_2094 ;
                                         tmprc_2094 = buf1_19[0];
                                         a_31 = tmprc_2094;
                                      } 
                                      {
                                        struct tuptyp_2067* tmpsmp_1785 ;
                                        {
                                          struct tuptyp_2067* tmprc_2093 ;
                                           tmprc_2093 = CDR(a_31, struct tuptyp_2067);
                                           tmpsmp_1785 = tmprc_2093;
                                        } 
                                        {
                                          struct tuptyp_2067* tmp_2091 ;
                                           tmp_2091 = buf1_19[0];
                                          {
                                            struct tuptyp_2067* tmp_2092 ;
                                             tmp_2092 = tmpsmp_1785;
                                            buf1_19[0] = tmp_2092;
                                          } 
                                        } 
                                      } 
                                    } 
                                  } 
                                } 
                                EMIT(a_23, struct tuptyp_2067, wsq_printer_1);
                                 tmpsmp_1793 = ((char)0);
                              } else {
                                {
                                  struct tuptyp_2067* a_28 ;
                                  {
                                    struct tuptyp_2067* tmprc_2090 ;
                                     tmprc_2090 = buf2_20[0];
                                     a_28 = tmprc_2090;
                                  } 
                                  {
                                    struct tuptyp_2067 x_26 ;
                                    {
                                      struct tuptyp_2067 tmprc_2089 ;
                                       tmprc_2089 = CAR(a_28, struct tuptyp_2067);
                                       x_26 = tmprc_2089;
                                    } 
                                    {
                                      struct tuptyp_2067* a_27 ;
                                      {
                                        struct tuptyp_2067* tmprc_2088 ;
                                         tmprc_2088 = buf2_20[0];
                                         a_27 = tmprc_2088;
                                      } 
                                      {
                                        struct tuptyp_2067* tmpsmp_1789 ;
                                        {
                                          struct tuptyp_2067* tmprc_2087 ;
                                           tmprc_2087 = CDR(a_27, struct tuptyp_2067);
                                           tmpsmp_1789 = tmprc_2087;
                                        } 
                                        {
                                          struct tuptyp_2067* tmp_2085 ;
                                           tmp_2085 = buf2_20[0];
                                          {
                                            struct tuptyp_2067* tmp_2086 ;
                                             tmp_2086 = tmpsmp_1789;
                                            buf2_20[0] = tmp_2086;
                                          } 
                                        } 
                                      } 
                                    } 
                                  } 
                                } 
                                EMIT(b_24, struct tuptyp_2067, wsq_printer_1);
                                 tmpsmp_1793 = ((char)0);
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
    } else break; 
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
void union2_3(struct Union2_1 arg_3725) {
  GRAB_WRITEFIFO(wsq_mergeMonotonic_2);
  EMIT(arg_3725, struct Union2_1, wsq_mergeMonotonic_2);
  RELEASE_WRITEFIFO(wsq_mergeMonotonic_2);
} 

void stream_map_4(struct tuptyp_2067 x_76) {
  char ___VIRTQUEUE____75;
  GRAB_WRITEFIFO(union2_3);
  {
    struct Union2_1 tmpsmp_1571 ;
    {
      struct Union2_1 tmprc_2270 ;
      struct tuptyp_2067 sumbld_3723 = x_76;
      struct Union2_1 sumresult_3724; // Casting to union parent type
      sumresult_3724.payload.Left_1 = sumbld_3723;
      sumresult_3724.tag = Left_1;
       tmprc_2270 = sumresult_3724;
       tmpsmp_1571 = tmprc_2270;
    } 
    EMIT(tmpsmp_1571, struct Union2_1, union2_3);
  } 
  #ifdef WS_DISJOINT_HEAPS
  {
      // Copy & Release enqueued FIFO contents.
      int i;
      int pending = wsfifo_pending(& union2_3_queue); 
      for(i=0; i < pending ; i++) { 
        void* ptr = wsfifo_recheck(& union2_3_queue);
        // COPY IT
    
        wsfifo_release_one(& union2_3_queue);
      }
  } 
  #endif
  RELEASE_WRITEFIFO(union2_3);
} 

void stream_map_5(struct tuptyp_2067 x_79) {
  char ___VIRTQUEUE____78;
  GRAB_WRITEFIFO(union2_3);
  {
    struct Union2_1 tmpsmp_1567 ;
    {
      struct Union2_1 tmprc_2272 ;
      struct tuptyp_2067 sumbld_3721 = x_79;
      struct Union2_1 sumresult_3722; // Casting to union parent type
      sumresult_3722.payload.Right_1 = sumbld_3721;
      sumresult_3722.tag = Right_1;
       tmprc_2272 = sumresult_3722;
       tmpsmp_1567 = tmprc_2272;
    } 
    EMIT(tmpsmp_1567, struct Union2_1, union2_3);
  } 
  #ifdef WS_DISJOINT_HEAPS
  {
      // Copy & Release enqueued FIFO contents.
      int i;
      int pending = wsfifo_pending(& union2_3_queue); 
      for(i=0; i < pending ; i++) { 
        void* ptr = wsfifo_recheck(& union2_3_queue);
        // COPY IT
    
        wsfifo_release_one(& union2_3_queue);
      }
  } 
  #endif
  RELEASE_WRITEFIFO(union2_3);
} 

void wsq_randomSource_6(char __85) {
  char ___VIRTQUEUE____84;
  GRAB_WRITEFIFO(stream_map_4);
  {
    int tmpsmp_1529 ;
     tmpsmp_1529 = ARRLEN(all_syms_82);
    {
      int i_86 ;
       i_86 = (rand() % tmpsmp_1529);
      {
        float tmpsmp_1553 ;
         tmpsmp_1553 = lastprice_83[i_86];
        {
          int tmpsmp_1555 ;
           tmpsmp_1555 = (rand() % 200);
          {
            int tmpsmp_1557 ;
             tmpsmp_1557 = (tmpsmp_1555 - 100);
            {
              float tmpsmp_1559 ;
               tmpsmp_1559 = (float)tmpsmp_1557;
              {
                float tmpsmp_1561 ;
                 tmpsmp_1561 = (tmpsmp_1559 / 100.0F);
                {
                  float tmpsmp_1563 ;
                   tmpsmp_1563 = (tmpsmp_1553 + tmpsmp_1561);
                  lastprice_83[i_86] = tmpsmp_1563;
                } 
              } 
            } 
          } 
        } 
      } 
      {
        float tmpsmp_1547 ;
         tmpsmp_1547 = lastprice_83[i_86];
        {
          ws_bool_t tmpsmp_1549 ;
           tmpsmp_1549 = (tmpsmp_1547 < 0.0F);
          {
            char ignored_valInEffect_1876 ;
            if (tmpsmp_1549) {
              lastprice_83[i_86] = 0.0F;
               ignored_valInEffect_1876 = ((char)0);
            } else {
               ignored_valInEffect_1876 = ((char)0);
            }
          } 
        } 
      } 
      {
        float tmpsmp_1537 ;
         tmpsmp_1537 = lastprice_83[i_86];
        {
          char* tmpsmp_1539 ;
          {
            char* tmprc_2776 ;
             tmprc_2776 = all_syms_82[i_86];
             tmpsmp_1539 = tmprc_2776;
          } 
          {
            int tmpsmp_1541 ;
             tmpsmp_1541 = (rand() % 10);
            {
              int tmpsmp_1543 ;
               tmpsmp_1543 = (tmpsmp_1541 + 1);
              {
                struct tuptyp_2067 tmpsmp_1545 ;
                {
                  struct tuptyp_2067 tmprc_2775 = {tmpsmp_1537, tmpsmp_1539, t_81, tmpsmp_1543};
                   tmpsmp_1545 = tmprc_2775;
                } 
                EMIT(tmpsmp_1545, struct tuptyp_2067, stream_map_4);
              } 
            } 
          } 
        } 
      } 
      {
        int tmpsmp_1531 ;
         tmpsmp_1531 = (rand() % 10);
        {
          float tmpsmp_1533 ;
           tmpsmp_1533 = (float)tmpsmp_1531;
          {
            float tmpsmp_1535 ;
             tmpsmp_1535 = (t_81 + tmpsmp_1533);
            t_81 = tmpsmp_1535;
          } 
        } 
      } 
    } 
  } 
  RELEASE_WRITEFIFO(stream_map_4);
} 

void socket_in_7(uint8_t* buf_88) {
  char ___VIRTQUEUE____87;
  GRAB_WRITEFIFO(stream_map_5);
  {
    int offset_1270 ;
    {
      int tmprc_2786 ;
       tmprc_2786 = 0;
       offset_1270 = tmprc_2786;
    } 
    {
      int tmpsmp_1503 ;
       tmpsmp_1503 = (offset_1270 + 4);
      offset_1270 = tmpsmp_1503;
    } 
    {
      int tmpsmp_1501 ;
       tmpsmp_1501 = (offset_1270 - 4);
      {
        float tupfld_1274 ;
         tupfld_1274 = (*((float *)(buf_88 + tmpsmp_1501))) /* type_unsafe_read */;
        {
          int tmpsmp_1507 ;
           tmpsmp_1507 = (offset_1270 + 4);
          offset_1270 = tmpsmp_1507;
        } 
        {
          int tmpsmp_1505 ;
           tmpsmp_1505 = (offset_1270 - 4);
          {
            int len_1275 ;
             len_1275 = (*((int *)(buf_88 + tmpsmp_1505))) /* type_unsafe_read */;
            {
              char* arr_1276 ;
              {
                char* tmprc_2785 ;
                int* arrtmp_3715 = (int*)0;
                if (len_1275 > 0) {
                  arrtmp_3715 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * len_1275) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
                  SETARRLEN(arrtmp_3715, len_1275);
                } 
                 tmprc_2785 = (char*)arrtmp_3715;
                 arr_1276 = tmprc_2785;
              } 
              {
                int tmpsmp_1509 ;
                 tmpsmp_1509 = (len_1275 - 1);
                {
                   int ind_1277;
                  for (ind_1277 = 0; ind_1277 <= tmpsmp_1509; ind_1277++) {
                    {
                      int tmpsmp_1513 ;
                       tmpsmp_1513 = (offset_1270 + 1);
                      offset_1270 = tmpsmp_1513;
                    } 
                    {
                      int tmpsmp_1511 ;
                       tmpsmp_1511 = (offset_1270 - 1);
                      {
                        char tmpsmp_1515 ;
                         tmpsmp_1515 = (*((char *)(buf_88 + tmpsmp_1511))) /* type_unsafe_read */;
                        arr_1276[ind_1277] = tmpsmp_1515;
                      } 
                    } 
                  } 
                } 
              } 
              {
                int tmpsmp_1521 ;
                 tmpsmp_1521 = (offset_1270 + 4);
                offset_1270 = tmpsmp_1521;
              } 
              {
                int tmpsmp_1519 ;
                 tmpsmp_1519 = (offset_1270 - 4);
                {
                  float tupfld_1272 ;
                   tupfld_1272 = (*((float *)(buf_88 + tmpsmp_1519))) /* type_unsafe_read */;
                  {
                    int tmpsmp_1525 ;
                     tmpsmp_1525 = (offset_1270 + 4);
                    offset_1270 = tmpsmp_1525;
                  } 
                  {
                    int tmpsmp_1523 ;
                     tmpsmp_1523 = (offset_1270 - 4);
                    {
                      int tupfld_1271 ;
                       tupfld_1271 = (*((int *)(buf_88 + tmpsmp_1523))) /* type_unsafe_read */;
                      {
                        struct tuptyp_2067 ob_89 ;
                        {
                          struct tuptyp_2067 tmprc_2782 = {tupfld_1274, arr_1276, tupfld_1272, tupfld_1271};
                           ob_89 = tmprc_2782;
                        } 
                        EMIT(ob_89, struct tuptyp_2067, stream_map_5);
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
      int pending = wsfifo_pending(& stream_map_5_queue); 
      for(i=0; i < pending ; i++) { 
        void* ptr = wsfifo_recheck(& stream_map_5_queue);
        // COPY IT
    
        wsfifo_release_one(& stream_map_5_queue);
      }
  } 
  #endif
  RELEASE_WRITEFIFO(stream_map_5);
} 

// merge operator: 
void socket_in_raw_9(uint8_t* arg_3714) {
  GRAB_WRITEFIFO(socket_in_7);
  EMIT(arg_3714, uint8_t*, socket_in_7);
  RELEASE_WRITEFIFO(socket_in_7);
} 

void socket_in_raw_10(char __93) {
  char ___VIRTQUEUE____92;
  GRAB_WRITEFIFO(socket_in_raw_9);
  {
    int64_t ignored_valInEffect_1877 ;
     ignored_valInEffect_1877 = spawn_socket_client_helper(tmpconstlift_1229, 9877) /* foreign app */ ;
  } 
  RELEASE_WRITEFIFO(socket_in_raw_9);
} 

void socket_in_raw_11(char __114) {
  char ___VIRTQUEUE____113;
  GRAB_WRITEFIFO(socket_in_raw_9);
  {
    ws_bool_t tmpsmp_1483 ;
     tmpsmp_1483 = (sockfd_97 == 0);
    {
      char ignored_valInEffect_1885 ;
      if (tmpsmp_1483) {
        {
          int tmpsmp_1493 ;
           tmpsmp_1493 = poll_socket_client_ready_port(9877) /* foreign app */ ;
          sockfd_97 = tmpsmp_1493;
        } 
        {
          ws_bool_t tmpsmp_1485 ;
           tmpsmp_1485 = (sockfd_97 == 0);
          {
            ws_bool_t tmpsmp_1487 ;
             tmpsmp_1487 = !(tmpsmp_1485);
            {
              char tmpsmp_1495 ;
              if (tmpsmp_1487) {
                connected_95 = TRUE;
                {
                  int tmpsmp_1489 ;
                   tmpsmp_1489 = ws_EWOULDBLOCK() /* foreign app */ ;
                  wouldblock_99 = tmpsmp_1489;
                   tmpsmp_1495 = ((char)0);
                } 
              } else {
                 tmpsmp_1495 = ((char)0);
              }
               ignored_valInEffect_1885 = tmpsmp_1495;
            } 
          } 
        } 
      } else {
         ignored_valInEffect_1885 = ((char)0);
      }
    } 
  } 
  {
    char ignored_valInEffect_1884 ;
    if (connected_95) {
      {
        ws_bool_t tmpsmp_1379 ;
         tmpsmp_1379 = (curstate_102 == 0);
        {
          char ignored_valInEffect_1883 ;
          if (tmpsmp_1379) {
            {
              int remaining_124 ;
               remaining_124 = (4 - lenctr_96);
              {
                int arg_135 ;
                 arg_135 = sockfd_97;
                {
                  int arg_133 ;
                   arg_133 = lenctr_96;
                  {
                    int rd_125 ;
                     rd_125 = ws_read_offset(arg_135, lenbuf_110, arg_133, remaining_124) /* foreign app */ ;
                    {
                      ws_bool_t tmpsmp_1427 ;
                       tmpsmp_1427 = (rd_125 == -1);
                      {
                        char ignored_valInEffect_1882 ;
                        if (tmpsmp_1427) {
                          {
                            int code_130 ;
                             code_130 = get_errno() /* foreign app */ ;
                            {
                              ws_bool_t tmpsmp_1429 ;
                               tmpsmp_1429 = (code_130 == wouldblock_99);
                              {
                                char tmpsmp_1477 ;
                                if (tmpsmp_1429) {
                                   tmpsmp_1477 = ((char)0);
                                } else {
                                  {
                                    char* strarr1_1257 ;
                                    {
                                      char* tmprc_2867 ;
                                      int* arrtmp_3706 = (int*)0;
                                      if (100 > 0) {
                                        arrtmp_3706 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 100) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
                                        SETARRLEN(arrtmp_3706, 100);
                                      } 
                                      char* str_3704 = (char*)arrtmp_3706;
                                      int realsize_3705 = snprintf(str_3704, 100, "%d", (int)code_130);
                                      if (realsize_3705 >= 100) { printf("Error, show overflowed fixed length 100 buffer\n"); exit(-1); }
                                      SETARRLEN(str_3704, realsize_3705 + 1); /* set virtual length */ 
                                       tmprc_2867 = str_3704;
                                       strarr1_1257 = tmprc_2867;
                                    } 
                                    {
                                      char* strarr2_1258 ;
                                      {
                                        char* tmprc_2869 ;
                                         tmprc_2869 = tmpconstlift_1234;
                                         strarr2_1258 = tmprc_2869;
                                      } 
                                      {
                                        int tmpsmp_1431 ;
                                         tmpsmp_1431 = ARRLEN(strarr1_1257);
                                        {
                                          int len_1261 ;
                                           len_1261 = (tmpsmp_1431 - 1);
                                          {
                                            int tmpsmp_1433 ;
                                             tmpsmp_1433 = ARRLEN(strarr2_1258);
                                            {
                                              int tmpsmp_1435 ;
                                               tmpsmp_1435 = (len_1261 + tmpsmp_1433);
                                              {
                                                char* appendresult_1262 ;
                                                {
                                                  char* tmprc_2875 ;
                                                  int* arrtmp_3703 = (int*)0;
                                                  if (tmpsmp_1435 > 0) {
                                                    arrtmp_3703 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * tmpsmp_1435) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
                                                    SETARRLEN(arrtmp_3703, tmpsmp_1435);
                                                  } 
                                                   tmprc_2875 = (char*)arrtmp_3703;
                                                   appendresult_1262 = tmprc_2875;
                                                } 
                                                {
                                                  int tmpsmp_1447 ;
                                                   tmpsmp_1447 = (len_1261 - 1);
                                                  {
                                                     int i_1259;
                                                    for (i_1259 = 0; i_1259 <= tmpsmp_1447; i_1259++) {
                                                      {
                                                        char tmpsmp_1449 ;
                                                         tmpsmp_1449 = strarr1_1257[i_1259];
                                                        appendresult_1262[i_1259] = tmpsmp_1449;
                                                      } 
                                                    } 
                                                  } 
                                                } 
                                                {
                                                  int tmpsmp_1437 ;
                                                   tmpsmp_1437 = ARRLEN(strarr2_1258);
                                                  {
                                                    int tmpsmp_1439 ;
                                                     tmpsmp_1439 = (tmpsmp_1437 - 1);
                                                    {
                                                       int i_1260;
                                                      for (i_1260 = 0; i_1260 <= tmpsmp_1439; i_1260++) {
                                                        {
                                                          int tmpsmp_1441 ;
                                                           tmpsmp_1441 = (len_1261 + i_1260);
                                                          {
                                                            char tmpsmp_1443 ;
                                                             tmpsmp_1443 = strarr2_1258[i_1260];
                                                            appendresult_1262[tmpsmp_1441] = tmpsmp_1443;
                                                          } 
                                                        } 
                                                      } 
                                                    } 
                                                  } 
                                                } 
                                                {
                                                  int tmpsmp_1453 ;
                                                   tmpsmp_1453 = ARRLEN(tmpconstlift_1235);
                                                  {
                                                    int len_1267 ;
                                                     len_1267 = (tmpsmp_1453 - 1);
                                                    {
                                                      int tmpsmp_1455 ;
                                                       tmpsmp_1455 = ARRLEN(appendresult_1262);
                                                      {
                                                        int tmpsmp_1457 ;
                                                         tmpsmp_1457 = (len_1267 + tmpsmp_1455);
                                                        {
                                                          char* appendresult_1268 ;
                                                          {
                                                            char* tmprc_2881 ;
                                                            int* arrtmp_3702 = (int*)0;
                                                            if (tmpsmp_1457 > 0) {
                                                              arrtmp_3702 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * tmpsmp_1457) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
                                                              SETARRLEN(arrtmp_3702, tmpsmp_1457);
                                                            } 
                                                             tmprc_2881 = (char*)arrtmp_3702;
                                                             appendresult_1268 = tmprc_2881;
                                                          } 
                                                          {
                                                            int tmpsmp_1469 ;
                                                             tmpsmp_1469 = (len_1267 - 1);
                                                            {
                                                               int i_1265;
                                                              for (i_1265 = 0; i_1265 <= tmpsmp_1469; i_1265++) {
                                                                {
                                                                  char tmpsmp_1471 ;
                                                                   tmpsmp_1471 = tmpconstlift_1235[i_1265];
                                                                  appendresult_1268[i_1265] = tmpsmp_1471;
                                                                } 
                                                              } 
                                                            } 
                                                          } 
                                                          {
                                                            int tmpsmp_1459 ;
                                                             tmpsmp_1459 = ARRLEN(appendresult_1262);
                                                            {
                                                              int tmpsmp_1461 ;
                                                               tmpsmp_1461 = (tmpsmp_1459 - 1);
                                                              {
                                                                 int i_1266;
                                                                for (i_1266 = 0; i_1266 <= tmpsmp_1461; i_1266++) {
                                                                  {
                                                                    int tmpsmp_1463 ;
                                                                     tmpsmp_1463 = (len_1267 + i_1266);
                                                                    {
                                                                      char tmpsmp_1465 ;
                                                                       tmpsmp_1465 = appendresult_1262[i_1266];
                                                                      appendresult_1268[tmpsmp_1463] = tmpsmp_1465;
                                                                    } 
                                                                  } 
                                                                } 
                                                              } 
                                                            } 
                                                          } 
                                                          wserror_builtin(appendresult_1268);
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
                                 ignored_valInEffect_1882 = tmpsmp_1477;
                              } 
                            } 
                          } 
                        } else {
                           ignored_valInEffect_1882 = ((char)0);
                        }
                      } 
                    } 
                    {
                      ws_bool_t tmpsmp_1381 ;
                       tmpsmp_1381 = (rd_125 == remaining_124);
                      {
                        char tmpsmp_1479 ;
                        if (tmpsmp_1381) {
                          {
                            int offset_1279 ;
                            {
                              int tmprc_2850 ;
                               tmprc_2850 = 0;
                               offset_1279 = tmprc_2850;
                            } 
                            {
                              int tmpsmp_1385 ;
                               tmpsmp_1385 = (offset_1279 + 4);
                              offset_1279 = tmpsmp_1385;
                            } 
                            {
                              int tmpsmp_1383 ;
                               tmpsmp_1383 = (offset_1279 - 4);
                              {
                                int tmp_126 ;
                                 tmp_126 = (*((int *)(lenbuf_110 + tmpsmp_1383))) /* type_unsafe_read */;
                                curstate_102 = 1;
                                msglen_100 = tmp_126;
                                {
                                  ws_bool_t tmpsmp_1389 ;
                                   tmpsmp_1389 = (msglen_100 < 0);
                                  {
                                    char ignored_valInEffect_1881 ;
                                    if (tmpsmp_1389) {
                                      {
                                        int tmpsmp_1391 ;
                                         tmpsmp_1391 = ARRLEN(tmpconstlift_1233);
                                        {
                                          int len_1255 ;
                                           len_1255 = (tmpsmp_1391 - 1);
                                          {
                                            int tmpsmp_1393 ;
                                             tmpsmp_1393 = ARRLEN(tmpconstlift_1232);
                                            {
                                              int tmpsmp_1395 ;
                                               tmpsmp_1395 = (len_1255 + tmpsmp_1393);
                                              {
                                                char* appendresult_1256 ;
                                                {
                                                  char* tmprc_2862 ;
                                                  int* arrtmp_3707 = (int*)0;
                                                  if (tmpsmp_1395 > 0) {
                                                    arrtmp_3707 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * tmpsmp_1395) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
                                                    SETARRLEN(arrtmp_3707, tmpsmp_1395);
                                                  } 
                                                   tmprc_2862 = (char*)arrtmp_3707;
                                                   appendresult_1256 = tmprc_2862;
                                                } 
                                                {
                                                  int tmpsmp_1407 ;
                                                   tmpsmp_1407 = (len_1255 - 1);
                                                  {
                                                     int i_1253;
                                                    for (i_1253 = 0; i_1253 <= tmpsmp_1407; i_1253++) {
                                                      {
                                                        char tmpsmp_1409 ;
                                                         tmpsmp_1409 = tmpconstlift_1233[i_1253];
                                                        appendresult_1256[i_1253] = tmpsmp_1409;
                                                      } 
                                                    } 
                                                  } 
                                                } 
                                                {
                                                  int tmpsmp_1397 ;
                                                   tmpsmp_1397 = ARRLEN(tmpconstlift_1232);
                                                  {
                                                    int tmpsmp_1399 ;
                                                     tmpsmp_1399 = (tmpsmp_1397 - 1);
                                                    {
                                                       int i_1254;
                                                      for (i_1254 = 0; i_1254 <= tmpsmp_1399; i_1254++) {
                                                        {
                                                          int tmpsmp_1401 ;
                                                           tmpsmp_1401 = (len_1255 + i_1254);
                                                          {
                                                            char tmpsmp_1403 ;
                                                             tmpsmp_1403 = tmpconstlift_1232[i_1254];
                                                            appendresult_1256[tmpsmp_1401] = tmpsmp_1403;
                                                          } 
                                                        } 
                                                      } 
                                                    } 
                                                  } 
                                                } 
                                                {
                                                  char ignored_valInEffect_1880 ;
                                                  puts_err(appendresult_1256);
                                                   ignored_valInEffect_1880 = ((char)0);
                                                } 
                                              } 
                                            } 
                                          } 
                                        } 
                                      } 
                                      {
                                        char ignored_valInEffect_1879 ;
                                        shutdown_sockets();
                                         ignored_valInEffect_1879 = ((char)0);
                                      } 
                                      {
                                        char tmpsmp_1413 ;
                                        wsexit_fun(0);
                                         tmpsmp_1413 = ((char)0);
                                         ignored_valInEffect_1881 = tmpsmp_1413;
                                      } 
                                    } else {
                                       ignored_valInEffect_1881 = ((char)0);
                                    }
                                  } 
                                } 
                                lenctr_96 = 0;
                                datactr_98 = 0;
                                {
                                  uint8_t* tmpsmp_1387 ;
                                  {
                                    uint8_t* tmprc_2856 ;
                                    int* arrtmp_3708 = (int*)0;
                                    if (msglen_100 > 0) {
                                      arrtmp_3708 = (int*)((char*)WSMALLOC_SCALAR((sizeof(uint8_t) * msglen_100) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
                                      SETARRLEN(arrtmp_3708, msglen_100);
                                    } 
                                     tmprc_2856 = (uint8_t*)arrtmp_3708;
                                     tmpsmp_1387 = tmprc_2856;
                                  } 
                                  {
                                    uint8_t* settmp_2855 ;
                                     settmp_2855 = databuf_101;
                                    databuf_101 = tmpsmp_1387;
                                  } 
                                } 
                                {
                                  char tmpsmp_1415 ;
                                   tmpsmp_1415 = ((char)0);
                                   tmpsmp_1479 = tmpsmp_1415;
                                } 
                              } 
                            } 
                          } 
                        } else {
                          {
                            ws_bool_t tmpsmp_1417 ;
                             tmpsmp_1417 = (rd_125 == 0);
                            {
                              char tmpsmp_1425 ;
                              if (tmpsmp_1417) {
                                 tmpsmp_1425 = ((char)0);
                              } else {
                                {
                                  int tmpsmp_1419 ;
                                   tmpsmp_1419 = (rd_125 > 0 ? rd_125 :0);
                                  {
                                    int tmpsmp_1421 ;
                                     tmpsmp_1421 = (lenctr_96 + tmpsmp_1419);
                                    lenctr_96 = tmpsmp_1421;
                                     tmpsmp_1425 = ((char)0);
                                  } 
                                } 
                              }
                               tmpsmp_1479 = tmpsmp_1425;
                            } 
                          } 
                        }
                         ignored_valInEffect_1883 = tmpsmp_1479;
                      } 
                    } 
                  } 
                } 
              } 
            } 
          } else {
             ignored_valInEffect_1883 = ((char)0);
          }
        } 
      } 
      {
        ws_bool_t tmpsmp_1309 ;
         tmpsmp_1309 = (curstate_102 == 1);
        {
          char tmpsmp_1481 ;
          if (tmpsmp_1309) {
            {
              int remaining_115 ;
               remaining_115 = (msglen_100 - datactr_98);
              {
                int arg_123 ;
                 arg_123 = sockfd_97;
                {
                  uint8_t* arg_122 ;
                   arg_122 = databuf_101;
                  {
                    int arg_121 ;
                     arg_121 = datactr_98;
                    {
                      int rd_116 ;
                       rd_116 = ws_read_offset(arg_123, arg_122, arg_121, remaining_115) /* foreign app */ ;
                      {
                        ws_bool_t tmpsmp_1325 ;
                         tmpsmp_1325 = (rd_116 == -1);
                        {
                          char ignored_valInEffect_1878 ;
                          if (tmpsmp_1325) {
                            {
                              int code_118 ;
                               code_118 = get_errno() /* foreign app */ ;
                              {
                                ws_bool_t tmpsmp_1327 ;
                                 tmpsmp_1327 = (code_118 == wouldblock_99);
                                {
                                  char tmpsmp_1375 ;
                                  if (tmpsmp_1327) {
                                     tmpsmp_1375 = ((char)0);
                                  } else {
                                    {
                                      char* strarr1_1239 ;
                                      {
                                        char* tmprc_2816 ;
                                        int* arrtmp_3713 = (int*)0;
                                        if (100 > 0) {
                                          arrtmp_3713 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 100) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
                                          SETARRLEN(arrtmp_3713, 100);
                                        } 
                                        char* str_3711 = (char*)arrtmp_3713;
                                        int realsize_3712 = snprintf(str_3711, 100, "%d", (int)code_118);
                                        if (realsize_3712 >= 100) { printf("Error, show overflowed fixed length 100 buffer\n"); exit(-1); }
                                        SETARRLEN(str_3711, realsize_3712 + 1); /* set virtual length */ 
                                         tmprc_2816 = str_3711;
                                         strarr1_1239 = tmprc_2816;
                                      } 
                                      {
                                        char* strarr2_1240 ;
                                        {
                                          char* tmprc_2818 ;
                                           tmprc_2818 = tmpconstlift_1230;
                                           strarr2_1240 = tmprc_2818;
                                        } 
                                        {
                                          int tmpsmp_1329 ;
                                           tmpsmp_1329 = ARRLEN(strarr1_1239);
                                          {
                                            int len_1243 ;
                                             len_1243 = (tmpsmp_1329 - 1);
                                            {
                                              int tmpsmp_1331 ;
                                               tmpsmp_1331 = ARRLEN(strarr2_1240);
                                              {
                                                int tmpsmp_1333 ;
                                                 tmpsmp_1333 = (len_1243 + tmpsmp_1331);
                                                {
                                                  char* appendresult_1244 ;
                                                  {
                                                    char* tmprc_2824 ;
                                                    int* arrtmp_3710 = (int*)0;
                                                    if (tmpsmp_1333 > 0) {
                                                      arrtmp_3710 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * tmpsmp_1333) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
                                                      SETARRLEN(arrtmp_3710, tmpsmp_1333);
                                                    } 
                                                     tmprc_2824 = (char*)arrtmp_3710;
                                                     appendresult_1244 = tmprc_2824;
                                                  } 
                                                  {
                                                    int tmpsmp_1345 ;
                                                     tmpsmp_1345 = (len_1243 - 1);
                                                    {
                                                       int i_1241;
                                                      for (i_1241 = 0; i_1241 <= tmpsmp_1345; i_1241++) {
                                                        {
                                                          char tmpsmp_1347 ;
                                                           tmpsmp_1347 = strarr1_1239[i_1241];
                                                          appendresult_1244[i_1241] = tmpsmp_1347;
                                                        } 
                                                      } 
                                                    } 
                                                  } 
                                                  {
                                                    int tmpsmp_1335 ;
                                                     tmpsmp_1335 = ARRLEN(strarr2_1240);
                                                    {
                                                      int tmpsmp_1337 ;
                                                       tmpsmp_1337 = (tmpsmp_1335 - 1);
                                                      {
                                                         int i_1242;
                                                        for (i_1242 = 0; i_1242 <= tmpsmp_1337; i_1242++) {
                                                          {
                                                            int tmpsmp_1339 ;
                                                             tmpsmp_1339 = (len_1243 + i_1242);
                                                            {
                                                              char tmpsmp_1341 ;
                                                               tmpsmp_1341 = strarr2_1240[i_1242];
                                                              appendresult_1244[tmpsmp_1339] = tmpsmp_1341;
                                                            } 
                                                          } 
                                                        } 
                                                      } 
                                                    } 
                                                  } 
                                                  {
                                                    int tmpsmp_1351 ;
                                                     tmpsmp_1351 = ARRLEN(tmpconstlift_1231);
                                                    {
                                                      int len_1249 ;
                                                       len_1249 = (tmpsmp_1351 - 1);
                                                      {
                                                        int tmpsmp_1353 ;
                                                         tmpsmp_1353 = ARRLEN(appendresult_1244);
                                                        {
                                                          int tmpsmp_1355 ;
                                                           tmpsmp_1355 = (len_1249 + tmpsmp_1353);
                                                          {
                                                            char* appendresult_1250 ;
                                                            {
                                                              char* tmprc_2830 ;
                                                              int* arrtmp_3709 = (int*)0;
                                                              if (tmpsmp_1355 > 0) {
                                                                arrtmp_3709 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * tmpsmp_1355) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
                                                                SETARRLEN(arrtmp_3709, tmpsmp_1355);
                                                              } 
                                                               tmprc_2830 = (char*)arrtmp_3709;
                                                               appendresult_1250 = tmprc_2830;
                                                            } 
                                                            {
                                                              int tmpsmp_1367 ;
                                                               tmpsmp_1367 = (len_1249 - 1);
                                                              {
                                                                 int i_1247;
                                                                for (i_1247 = 0; i_1247 <= tmpsmp_1367; i_1247++) {
                                                                  {
                                                                    char tmpsmp_1369 ;
                                                                     tmpsmp_1369 = tmpconstlift_1231[i_1247];
                                                                    appendresult_1250[i_1247] = tmpsmp_1369;
                                                                  } 
                                                                } 
                                                              } 
                                                            } 
                                                            {
                                                              int tmpsmp_1357 ;
                                                               tmpsmp_1357 = ARRLEN(appendresult_1244);
                                                              {
                                                                int tmpsmp_1359 ;
                                                                 tmpsmp_1359 = (tmpsmp_1357 - 1);
                                                                {
                                                                   int i_1248;
                                                                  for (i_1248 = 0; i_1248 <= tmpsmp_1359; i_1248++) {
                                                                    {
                                                                      int tmpsmp_1361 ;
                                                                       tmpsmp_1361 = (len_1249 + i_1248);
                                                                      {
                                                                        char tmpsmp_1363 ;
                                                                         tmpsmp_1363 = appendresult_1244[i_1248];
                                                                        appendresult_1250[tmpsmp_1361] = tmpsmp_1363;
                                                                      } 
                                                                    } 
                                                                  } 
                                                                } 
                                                              } 
                                                            } 
                                                            wserror_builtin(appendresult_1250);
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
                                   ignored_valInEffect_1878 = tmpsmp_1375;
                                } 
                              } 
                            } 
                          } else {
                             ignored_valInEffect_1878 = ((char)0);
                          }
                        } 
                      } 
                      {
                        ws_bool_t tmpsmp_1311 ;
                         tmpsmp_1311 = (rd_116 == remaining_115);
                        {
                          char tmpsmp_1377 ;
                          if (tmpsmp_1311) {
                            EMIT(databuf_101, uint8_t*, socket_in_raw_9);
                            {
                              uint8_t* settmp_2811 ;
                               settmp_2811 = databuf_101;
                              databuf_101 = ((uint8_t*)0) /* Array:null */;
                            } 
                            curstate_102 = 0;
                            lenctr_96 = 0;
                            datactr_98 = 0;
                            msglen_100 = 0;
                             tmpsmp_1377 = ((char)0);
                          } else {
                            {
                              ws_bool_t tmpsmp_1315 ;
                               tmpsmp_1315 = (rd_116 == 0);
                              {
                                char tmpsmp_1323 ;
                                if (tmpsmp_1315) {
                                   tmpsmp_1323 = ((char)0);
                                } else {
                                  {
                                    int tmpsmp_1317 ;
                                     tmpsmp_1317 = (rd_116 > 0 ? rd_116 :0);
                                    {
                                      int tmpsmp_1319 ;
                                       tmpsmp_1319 = (datactr_98 + tmpsmp_1317);
                                      datactr_98 = tmpsmp_1319;
                                       tmpsmp_1323 = ((char)0);
                                    } 
                                  } 
                                }
                                 tmpsmp_1377 = tmpsmp_1323;
                              } 
                            } 
                          }
                           tmpsmp_1481 = tmpsmp_1377;
                        } 
                      } 
                    } 
                  } 
                } 
              } 
            } 
          } else {
             tmpsmp_1481 = ((char)0);
          }
           ignored_valInEffect_1884 = tmpsmp_1481;
        } 
      } 
    } else {
       ignored_valInEffect_1884 = ((char)0);
    }
  } 
  RELEASE_WRITEFIFO(socket_in_raw_9);
} 


void initState() {
  /* We may need to start up the Boehm GC or do other standard WS init: */ 
  wsInternalInit();
  TOTAL_WORKERS(12);
  // [2008.11.07] The static data gets allocated against a never-cleared ZCT: 
  #ifdef WS_THREADED 
  #ifdef WS_USE_ZCT 
   zct_t* zct = WSCALLOC(sizeof(zct_t), 1);
  #endif
  #endif
  REGISTER_WORKER(0, char, BASE);
  REGISTER_WORKER(1, char, socket_in_raw_11);
  REGISTER_WORKER(2, char, socket_in_raw_10);
  REGISTER_WORKER(3, uint8_t*, socket_in_raw_9);
  REGISTER_WORKER(4, uint8_t*, socket_in_7);
  REGISTER_WORKER(5, char, wsq_randomSource_6);
  REGISTER_WORKER(6, struct tuptyp_2067, stream_map_5);
  REGISTER_WORKER(7, struct tuptyp_2067, stream_map_4);
  REGISTER_WORKER(8, struct Union2_1, union2_3);
  REGISTER_WORKER(9, struct Union2_1, wsq_mergeMonotonic_2);
  REGISTER_WORKER(10, struct tuptyp_2067, wsq_printer_1);
  REGISTER_WORKER(11, char, tmpsmp_1871);
  {
    char* tmprc_3162 ;
    int* arrtmp_3700 = (int*)0;
    if (2 > 0) {
      arrtmp_3700 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3700, 2);
    } 
    char* tmpchararr_3699 = (char*)arrtmp_3700;
    memcpy(tmpchararr_3699, "(", 2);
     tmprc_3162 = tmpchararr_3699;
     tmpconstlift_1238 = tmprc_3162;
  } 
  {
    char* tmprc_3161 ;
    int* arrtmp_3698 = (int*)0;
    if (2 > 0) {
      arrtmp_3698 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3698, 2);
    } 
    char* tmpchararr_3697 = (char*)arrtmp_3698;
    memcpy(tmpchararr_3697, ")", 2);
     tmprc_3161 = tmpchararr_3697;
     tmpconstlift_1237 = tmprc_3161;
  } 
  {
    char* tmprc_3160 ;
    int* arrtmp_3696 = (int*)0;
    if (2 > 0) {
      arrtmp_3696 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3696, 2);
    } 
    char* tmpchararr_3695 = (char*)arrtmp_3696;
    memcpy(tmpchararr_3695, "\n", 2);
     tmprc_3160 = tmpchararr_3695;
     tmpconstlift_1236 = tmprc_3160;
  } 
  {
    char* tmprc_3159 ;
    int* arrtmp_3694 = (int*)0;
    if (44 > 0) {
      arrtmp_3694 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 44) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3694, 44);
    } 
    char* tmpchararr_3693 = (char*)arrtmp_3694;
    memcpy(tmpchararr_3693, "  <socket.ws> ERROR: read() returned errno ", 44);
     tmprc_3159 = tmpchararr_3693;
     tmpconstlift_1235 = tmprc_3159;
  } 
  {
    char* tmprc_3158 ;
    int* arrtmp_3692 = (int*)0;
    if (2 > 0) {
      arrtmp_3692 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3692, 2);
    } 
    char* tmpchararr_3691 = (char*)arrtmp_3692;
    memcpy(tmpchararr_3691, "\n", 2);
     tmprc_3158 = tmpchararr_3691;
     tmpconstlift_1234 = tmprc_3158;
  } 
  {
    char* tmprc_3157 ;
    int* arrtmp_3690 = (int*)0;
    if (66 > 0) {
      arrtmp_3690 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 66) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3690, 66);
    } 
    char* tmpchararr_3689 = (char*)arrtmp_3690;
    memcpy(tmpchararr_3689, " <socket.ws> Got special EOS header.  Shutting down immediately. ", 66);
     tmprc_3157 = tmpchararr_3689;
     tmpconstlift_1233 = tmprc_3157;
  } 
  {
    char* tmprc_3156 ;
    int* arrtmp_3688 = (int*)0;
    if (62 > 0) {
      arrtmp_3688 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 62) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3688, 62);
    } 
    char* tmpchararr_3687 = (char*)arrtmp_3688;
    memcpy(tmpchararr_3687, " TODO!!!: should wait for other sockets/streams to finish...\n", 62);
     tmprc_3156 = tmpchararr_3687;
     tmpconstlift_1232 = tmprc_3156;
  } 
  {
    char* tmprc_3155 ;
    int* arrtmp_3686 = (int*)0;
    if (44 > 0) {
      arrtmp_3686 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 44) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3686, 44);
    } 
    char* tmpchararr_3685 = (char*)arrtmp_3686;
    memcpy(tmpchararr_3685, "  <socket.ws> ERROR: read() returned errno ", 44);
     tmprc_3155 = tmpchararr_3685;
     tmpconstlift_1231 = tmprc_3155;
  } 
  {
    char* tmprc_3154 ;
    int* arrtmp_3684 = (int*)0;
    if (2 > 0) {
      arrtmp_3684 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3684, 2);
    } 
    char* tmpchararr_3683 = (char*)arrtmp_3684;
    memcpy(tmpchararr_3683, "\n", 2);
     tmprc_3154 = tmpchararr_3683;
     tmpconstlift_1230 = tmprc_3154;
  } 
  {
    char* tmprc_3153 ;
    int* arrtmp_3682 = (int*)0;
    if (10 > 0) {
      arrtmp_3682 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 10) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3682, 10);
    } 
    char* tmpchararr_3681 = (char*)arrtmp_3682;
    memcpy(tmpchararr_3681, "localhost", 10);
     tmprc_3153 = tmpchararr_3681;
     tmpconstlift_1229 = tmprc_3153;
  } 
  {
    char* tmprc_3152 ;
    int* arrtmp_3680 = (int*)0;
    if (4 > 0) {
      arrtmp_3680 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3680, 4);
    } 
    char* tmpchararr_3679 = (char*)arrtmp_3680;
    memcpy(tmpchararr_3679, "IBM", 4);
     tmprc_3152 = tmpchararr_3679;
     tmpconstlift_1228 = tmprc_3152;
  } 
  {
    char* tmprc_3151 ;
    int* arrtmp_3678 = (int*)0;
    if (5 > 0) {
      arrtmp_3678 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3678, 5);
    } 
    char* tmpchararr_3677 = (char*)arrtmp_3678;
    memcpy(tmpchararr_3677, "GOOG", 5);
     tmprc_3151 = tmpchararr_3677;
     tmpconstlift_1227 = tmprc_3151;
  } 
  {
    char* tmprc_3150 ;
    int* arrtmp_3676 = (int*)0;
    if (3 > 0) {
      arrtmp_3676 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 3) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3676, 3);
    } 
    char* tmpchararr_3675 = (char*)arrtmp_3676;
    memcpy(tmpchararr_3675, "GM", 3);
     tmprc_3150 = tmpchararr_3675;
     tmpconstlift_1226 = tmprc_3150;
  } 
  {
    char* tmprc_3149 ;
    int* arrtmp_3674 = (int*)0;
    if (2 > 0) {
      arrtmp_3674 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3674, 2);
    } 
    char* tmpchararr_3673 = (char*)arrtmp_3674;
    memcpy(tmpchararr_3673, "F", 2);
     tmprc_3149 = tmpchararr_3673;
     tmpconstlift_1225 = tmprc_3149;
  } 
  {
    char* tmprc_3148 ;
    int* arrtmp_3672 = (int*)0;
    if (5 > 0) {
      arrtmp_3672 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3672, 5);
    } 
    char* tmpchararr_3671 = (char*)arrtmp_3672;
    memcpy(tmpchararr_3671, "IMGN", 5);
     tmprc_3148 = tmpchararr_3671;
     tmpconstlift_1224 = tmprc_3148;
  } 
  {
    char* tmprc_3147 ;
    int* arrtmp_3670 = (int*)0;
    if (5 > 0) {
      arrtmp_3670 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3670, 5);
    } 
    char* tmpchararr_3669 = (char*)arrtmp_3670;
    memcpy(tmpchararr_3669, "MSFT", 5);
     tmprc_3147 = tmpchararr_3669;
     tmpconstlift_1223 = tmprc_3147;
  } 
  {
    char* tmprc_3146 ;
    int* arrtmp_3668 = (int*)0;
    if (5 > 0) {
      arrtmp_3668 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3668, 5);
    } 
    char* tmpchararr_3667 = (char*)arrtmp_3668;
    memcpy(tmpchararr_3667, "AAPL", 5);
     tmprc_3146 = tmpchararr_3667;
     tmpconstlift_1222 = tmprc_3146;
  } 
  {
    char* tmprc_3145 ;
    int* arrtmp_3666 = (int*)0;
    if (6 > 0) {
      arrtmp_3666 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 6) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3666, 6);
    } 
    char* tmpchararr_3665 = (char*)arrtmp_3666;
    memcpy(tmpchararr_3665, "AAUKY", 6);
     tmprc_3145 = tmpchararr_3665;
     tmpconstlift_1221 = tmprc_3145;
  } 
  {
    char* tmprc_3144 ;
    int* arrtmp_3664 = (int*)0;
    if (4 > 0) {
      arrtmp_3664 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3664, 4);
    } 
    char* tmpchararr_3663 = (char*)arrtmp_3664;
    memcpy(tmpchararr_3663, "AAV", 4);
     tmprc_3144 = tmpchararr_3663;
     tmpconstlift_1220 = tmprc_3144;
  } 
  {
    char* tmprc_3143 ;
    int* arrtmp_3662 = (int*)0;
    if (5 > 0) {
      arrtmp_3662 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3662, 5);
    } 
    char* tmpchararr_3661 = (char*)arrtmp_3662;
    memcpy(tmpchararr_3661, "AAWW", 5);
     tmprc_3143 = tmpchararr_3661;
     tmpconstlift_1219 = tmprc_3143;
  } 
  {
    char* tmprc_3142 ;
    int* arrtmp_3660 = (int*)0;
    if (3 > 0) {
      arrtmp_3660 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 3) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3660, 3);
    } 
    char* tmpchararr_3659 = (char*)arrtmp_3660;
    memcpy(tmpchararr_3659, "AB", 3);
     tmprc_3142 = tmpchararr_3659;
     tmpconstlift_1218 = tmprc_3142;
  } 
  {
    char* tmprc_3141 ;
    int* arrtmp_3658 = (int*)0;
    if (5 > 0) {
      arrtmp_3658 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3658, 5);
    } 
    char* tmpchararr_3657 = (char*)arrtmp_3658;
    memcpy(tmpchararr_3657, "ABAX", 5);
     tmprc_3141 = tmpchararr_3657;
     tmpconstlift_1217 = tmprc_3141;
  } 
  {
    char* tmprc_3140 ;
    int* arrtmp_3656 = (int*)0;
    if (4 > 0) {
      arrtmp_3656 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3656, 4);
    } 
    char* tmpchararr_3655 = (char*)arrtmp_3656;
    memcpy(tmpchararr_3655, "ABB", 4);
     tmprc_3140 = tmpchararr_3655;
     tmpconstlift_1216 = tmprc_3140;
  } 
  {
    char* tmprc_3139 ;
    int* arrtmp_3654 = (int*)0;
    if (4 > 0) {
      arrtmp_3654 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3654, 4);
    } 
    char* tmpchararr_3653 = (char*)arrtmp_3654;
    memcpy(tmpchararr_3653, "ABC", 4);
     tmprc_3139 = tmpchararr_3653;
     tmpconstlift_1215 = tmprc_3139;
  } 
  {
    char* tmprc_3138 ;
    int* arrtmp_3652 = (int*)0;
    if (5 > 0) {
      arrtmp_3652 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3652, 5);
    } 
    char* tmpchararr_3651 = (char*)arrtmp_3652;
    memcpy(tmpchararr_3651, "ABFS", 5);
     tmprc_3138 = tmpchararr_3651;
     tmpconstlift_1214 = tmprc_3138;
  } 
  {
    char* tmprc_3137 ;
    int* arrtmp_3650 = (int*)0;
    if (4 > 0) {
      arrtmp_3650 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3650, 4);
    } 
    char* tmpchararr_3649 = (char*)arrtmp_3650;
    memcpy(tmpchararr_3649, "ABG", 4);
     tmprc_3137 = tmpchararr_3649;
     tmpconstlift_1213 = tmprc_3137;
  } 
  {
    char* tmprc_3136 ;
    int* arrtmp_3648 = (int*)0;
    if (4 > 0) {
      arrtmp_3648 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3648, 4);
    } 
    char* tmpchararr_3647 = (char*)arrtmp_3648;
    memcpy(tmpchararr_3647, "ABM", 4);
     tmprc_3136 = tmpchararr_3647;
     tmpconstlift_1212 = tmprc_3136;
  } 
  {
    char* tmprc_3135 ;
    int* arrtmp_3646 = (int*)0;
    if (5 > 0) {
      arrtmp_3646 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3646, 5);
    } 
    char* tmpchararr_3645 = (char*)arrtmp_3646;
    memcpy(tmpchararr_3645, "ABMD", 5);
     tmprc_3135 = tmpchararr_3645;
     tmpconstlift_1211 = tmprc_3135;
  } 
  {
    char* tmprc_3134 ;
    int* arrtmp_3644 = (int*)0;
    if (4 > 0) {
      arrtmp_3644 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3644, 4);
    } 
    char* tmpchararr_3643 = (char*)arrtmp_3644;
    memcpy(tmpchararr_3643, "ABT", 4);
     tmprc_3134 = tmpchararr_3643;
     tmpconstlift_1210 = tmprc_3134;
  } 
  {
    char* tmprc_3133 ;
    int* arrtmp_3642 = (int*)0;
    if (4 > 0) {
      arrtmp_3642 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3642, 4);
    } 
    char* tmpchararr_3641 = (char*)arrtmp_3642;
    memcpy(tmpchararr_3641, "ABV", 4);
     tmprc_3133 = tmpchararr_3641;
     tmpconstlift_1209 = tmprc_3133;
  } 
  {
    char* tmprc_3132 ;
    int* arrtmp_3640 = (int*)0;
    if (5 > 0) {
      arrtmp_3640 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3640, 5);
    } 
    char* tmpchararr_3639 = (char*)arrtmp_3640;
    memcpy(tmpchararr_3639, "ABVT", 5);
     tmprc_3132 = tmpchararr_3639;
     tmpconstlift_1208 = tmprc_3132;
  } 
  {
    char* tmprc_3131 ;
    int* arrtmp_3638 = (int*)0;
    if (4 > 0) {
      arrtmp_3638 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3638, 4);
    } 
    char* tmpchararr_3637 = (char*)arrtmp_3638;
    memcpy(tmpchararr_3637, "ABX", 4);
     tmprc_3131 = tmpchararr_3637;
     tmpconstlift_1207 = tmprc_3131;
  } 
  {
    char* tmprc_3130 ;
    int* arrtmp_3636 = (int*)0;
    if (4 > 0) {
      arrtmp_3636 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3636, 4);
    } 
    char* tmpchararr_3635 = (char*)arrtmp_3636;
    memcpy(tmpchararr_3635, "ACC", 4);
     tmprc_3130 = tmpchararr_3635;
     tmpconstlift_1206 = tmprc_3130;
  } 
  {
    char* tmprc_3129 ;
    int* arrtmp_3634 = (int*)0;
    if (5 > 0) {
      arrtmp_3634 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3634, 5);
    } 
    char* tmpchararr_3633 = (char*)arrtmp_3634;
    memcpy(tmpchararr_3633, "ACCL", 5);
     tmprc_3129 = tmpchararr_3633;
     tmpconstlift_1205 = tmprc_3129;
  } 
  {
    char* tmprc_3128 ;
    int* arrtmp_3632 = (int*)0;
    if (4 > 0) {
      arrtmp_3632 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3632, 4);
    } 
    char* tmpchararr_3631 = (char*)arrtmp_3632;
    memcpy(tmpchararr_3631, "ACE", 4);
     tmprc_3128 = tmpchararr_3631;
     tmpconstlift_1204 = tmprc_3128;
  } 
  {
    char* tmprc_3127 ;
    int* arrtmp_3630 = (int*)0;
    if (5 > 0) {
      arrtmp_3630 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3630, 5);
    } 
    char* tmpchararr_3629 = (char*)arrtmp_3630;
    memcpy(tmpchararr_3629, "ACET", 5);
     tmprc_3127 = tmpchararr_3629;
     tmpconstlift_1203 = tmprc_3127;
  } 
  {
    char* tmprc_3126 ;
    int* arrtmp_3628 = (int*)0;
    if (4 > 0) {
      arrtmp_3628 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3628, 4);
    } 
    char* tmpchararr_3627 = (char*)arrtmp_3628;
    memcpy(tmpchararr_3627, "ACF", 4);
     tmprc_3126 = tmpchararr_3627;
     tmpconstlift_1202 = tmprc_3126;
  } 
  {
    char* tmprc_3125 ;
    int* arrtmp_3626 = (int*)0;
    if (5 > 0) {
      arrtmp_3626 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3626, 5);
    } 
    char* tmpchararr_3625 = (char*)arrtmp_3626;
    memcpy(tmpchararr_3625, "ACGL", 5);
     tmprc_3125 = tmpchararr_3625;
     tmpconstlift_1201 = tmprc_3125;
  } 
  {
    char* tmprc_3124 ;
    int* arrtmp_3624 = (int*)0;
    if (5 > 0) {
      arrtmp_3624 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3624, 5);
    } 
    char* tmpchararr_3623 = (char*)arrtmp_3624;
    memcpy(tmpchararr_3623, "ACGY", 5);
     tmprc_3124 = tmpchararr_3623;
     tmpconstlift_1200 = tmprc_3124;
  } 
  {
    char* tmprc_3123 ;
    int* arrtmp_3622 = (int*)0;
    if (4 > 0) {
      arrtmp_3622 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3622, 4);
    } 
    char* tmpchararr_3621 = (char*)arrtmp_3622;
    memcpy(tmpchararr_3621, "ACH", 4);
     tmprc_3123 = tmpchararr_3621;
     tmpconstlift_1199 = tmprc_3123;
  } 
  {
    char* tmprc_3122 ;
    int* arrtmp_3620 = (int*)0;
    if (4 > 0) {
      arrtmp_3620 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3620, 4);
    } 
    char* tmpchararr_3619 = (char*)arrtmp_3620;
    memcpy(tmpchararr_3619, "ACI", 4);
     tmprc_3122 = tmpchararr_3619;
     tmpconstlift_1198 = tmprc_3122;
  } 
  {
    char* tmprc_3121 ;
    int* arrtmp_3618 = (int*)0;
    if (5 > 0) {
      arrtmp_3618 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3618, 5);
    } 
    char* tmpchararr_3617 = (char*)arrtmp_3618;
    memcpy(tmpchararr_3617, "ACIW", 5);
     tmprc_3121 = tmpchararr_3617;
     tmpconstlift_1197 = tmprc_3121;
  } 
  {
    char* tmprc_3120 ;
    int* arrtmp_3616 = (int*)0;
    if (4 > 0) {
      arrtmp_3616 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3616, 4);
    } 
    char* tmpchararr_3615 = (char*)arrtmp_3616;
    memcpy(tmpchararr_3615, "ACL", 4);
     tmprc_3120 = tmpchararr_3615;
     tmpconstlift_1196 = tmprc_3120;
  } 
  {
    char* tmprc_3119 ;
    int* arrtmp_3614 = (int*)0;
    if (5 > 0) {
      arrtmp_3614 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3614, 5);
    } 
    char* tmpchararr_3613 = (char*)arrtmp_3614;
    memcpy(tmpchararr_3613, "ACLI", 5);
     tmprc_3119 = tmpchararr_3613;
     tmpconstlift_1195 = tmprc_3119;
  } 
  {
    char* tmprc_3118 ;
    int* arrtmp_3612 = (int*)0;
    if (4 > 0) {
      arrtmp_3612 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3612, 4);
    } 
    char* tmpchararr_3611 = (char*)arrtmp_3612;
    memcpy(tmpchararr_3611, "ACM", 4);
     tmprc_3118 = tmpchararr_3611;
     tmpconstlift_1194 = tmprc_3118;
  } 
  {
    char* tmprc_3117 ;
    int* arrtmp_3610 = (int*)0;
    if (4 > 0) {
      arrtmp_3610 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3610, 4);
    } 
    char* tmpchararr_3609 = (char*)arrtmp_3610;
    memcpy(tmpchararr_3609, "ACN", 4);
     tmprc_3117 = tmpchararr_3609;
     tmpconstlift_1193 = tmprc_3117;
  } 
  {
    char* tmprc_3116 ;
    int* arrtmp_3608 = (int*)0;
    if (4 > 0) {
      arrtmp_3608 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3608, 4);
    } 
    char* tmpchararr_3607 = (char*)arrtmp_3608;
    memcpy(tmpchararr_3607, "ACO", 4);
     tmprc_3116 = tmpchararr_3607;
     tmpconstlift_1192 = tmprc_3116;
  } 
  {
    char* tmprc_3115 ;
    int* arrtmp_3606 = (int*)0;
    if (5 > 0) {
      arrtmp_3606 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3606, 5);
    } 
    char* tmpchararr_3605 = (char*)arrtmp_3606;
    memcpy(tmpchararr_3605, "ACOM", 5);
     tmprc_3115 = tmpchararr_3605;
     tmpconstlift_1191 = tmprc_3115;
  } 
  {
    char* tmprc_3114 ;
    int* arrtmp_3604 = (int*)0;
    if (5 > 0) {
      arrtmp_3604 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3604, 5);
    } 
    char* tmpchararr_3603 = (char*)arrtmp_3604;
    memcpy(tmpchararr_3603, "ACOR", 5);
     tmprc_3114 = tmpchararr_3603;
     tmpconstlift_1190 = tmprc_3114;
  } 
  {
    char* tmprc_3113 ;
    int* arrtmp_3602 = (int*)0;
    if (5 > 0) {
      arrtmp_3602 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3602, 5);
    } 
    char* tmpchararr_3601 = (char*)arrtmp_3602;
    memcpy(tmpchararr_3601, "ACTG", 5);
     tmprc_3113 = tmpchararr_3601;
     tmpconstlift_1189 = tmprc_3113;
  } 
  {
    char* tmprc_3112 ;
    int* arrtmp_3600 = (int*)0;
    if (4 > 0) {
      arrtmp_3600 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3600, 4);
    } 
    char* tmpchararr_3599 = (char*)arrtmp_3600;
    memcpy(tmpchararr_3599, "ACV", 4);
     tmprc_3112 = tmpchararr_3599;
     tmpconstlift_1188 = tmprc_3112;
  } 
  {
    char* tmprc_3111 ;
    int* arrtmp_3598 = (int*)0;
    if (5 > 0) {
      arrtmp_3598 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3598, 5);
    } 
    char* tmpchararr_3597 = (char*)arrtmp_3598;
    memcpy(tmpchararr_3597, "ACXM", 5);
     tmprc_3111 = tmpchararr_3597;
     tmpconstlift_1187 = tmprc_3111;
  } 
  {
    char* tmprc_3110 ;
    int* arrtmp_3596 = (int*)0;
    if (5 > 0) {
      arrtmp_3596 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3596, 5);
    } 
    char* tmpchararr_3595 = (char*)arrtmp_3596;
    memcpy(tmpchararr_3595, "ADBE", 5);
     tmprc_3110 = tmpchararr_3595;
     tmpconstlift_1186 = tmprc_3110;
  } 
  {
    char* tmprc_3109 ;
    int* arrtmp_3594 = (int*)0;
    if (5 > 0) {
      arrtmp_3594 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3594, 5);
    } 
    char* tmpchararr_3593 = (char*)arrtmp_3594;
    memcpy(tmpchararr_3593, "ADCT", 5);
     tmprc_3109 = tmpchararr_3593;
     tmpconstlift_1185 = tmprc_3109;
  } 
  {
    char* tmprc_3108 ;
    int* arrtmp_3592 = (int*)0;
    if (4 > 0) {
      arrtmp_3592 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3592, 4);
    } 
    char* tmpchararr_3591 = (char*)arrtmp_3592;
    memcpy(tmpchararr_3591, "ADI", 4);
     tmprc_3108 = tmpchararr_3591;
     tmpconstlift_1184 = tmprc_3108;
  } 
  {
    char* tmprc_3107 ;
    int* arrtmp_3590 = (int*)0;
    if (4 > 0) {
      arrtmp_3590 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3590, 4);
    } 
    char* tmpchararr_3589 = (char*)arrtmp_3590;
    memcpy(tmpchararr_3589, "ADM", 4);
     tmprc_3107 = tmpchararr_3589;
     tmpconstlift_1183 = tmprc_3107;
  } 
  {
    char* tmprc_3106 ;
    int* arrtmp_3588 = (int*)0;
    if (4 > 0) {
      arrtmp_3588 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3588, 4);
    } 
    char* tmpchararr_3587 = (char*)arrtmp_3588;
    memcpy(tmpchararr_3587, "ADP", 4);
     tmprc_3106 = tmpchararr_3587;
     tmpconstlift_1182 = tmprc_3106;
  } 
  {
    char* tmprc_3105 ;
    int* arrtmp_3586 = (int*)0;
    if (5 > 0) {
      arrtmp_3586 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3586, 5);
    } 
    char* tmpchararr_3585 = (char*)arrtmp_3586;
    memcpy(tmpchararr_3585, "ADRE", 5);
     tmprc_3105 = tmpchararr_3585;
     tmpconstlift_1181 = tmprc_3105;
  } 
  {
    char* tmprc_3104 ;
    int* arrtmp_3584 = (int*)0;
    if (4 > 0) {
      arrtmp_3584 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3584, 4);
    } 
    char* tmpchararr_3583 = (char*)arrtmp_3584;
    memcpy(tmpchararr_3583, "ADS", 4);
     tmprc_3104 = tmpchararr_3583;
     tmpconstlift_1180 = tmprc_3104;
  } 
  {
    char* tmprc_3103 ;
    int* arrtmp_3582 = (int*)0;
    if (5 > 0) {
      arrtmp_3582 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3582, 5);
    } 
    char* tmpchararr_3581 = (char*)arrtmp_3582;
    memcpy(tmpchararr_3581, "ADSK", 5);
     tmprc_3103 = tmpchararr_3581;
     tmpconstlift_1179 = tmprc_3103;
  } 
  {
    char* tmprc_3102 ;
    int* arrtmp_3580 = (int*)0;
    if (5 > 0) {
      arrtmp_3580 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3580, 5);
    } 
    char* tmpchararr_3579 = (char*)arrtmp_3580;
    memcpy(tmpchararr_3579, "ADTN", 5);
     tmprc_3102 = tmpchararr_3579;
     tmpconstlift_1178 = tmprc_3102;
  } 
  {
    char* tmprc_3101 ;
    int* arrtmp_3578 = (int*)0;
    if (5 > 0) {
      arrtmp_3578 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3578, 5);
    } 
    char* tmpchararr_3577 = (char*)arrtmp_3578;
    memcpy(tmpchararr_3577, "ADVS", 5);
     tmprc_3101 = tmpchararr_3577;
     tmpconstlift_1177 = tmprc_3101;
  } 
  {
    char* tmprc_3100 ;
    int* arrtmp_3576 = (int*)0;
    if (4 > 0) {
      arrtmp_3576 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3576, 4);
    } 
    char* tmpchararr_3575 = (char*)arrtmp_3576;
    memcpy(tmpchararr_3575, "ADY", 4);
     tmprc_3100 = tmpchararr_3575;
     tmpconstlift_1176 = tmprc_3100;
  } 
  {
    char* tmprc_3099 ;
    int* arrtmp_3574 = (int*)0;
    if (4 > 0) {
      arrtmp_3574 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3574, 4);
    } 
    char* tmpchararr_3573 = (char*)arrtmp_3574;
    memcpy(tmpchararr_3573, "AEE", 4);
     tmprc_3099 = tmpchararr_3573;
     tmpconstlift_1175 = tmprc_3099;
  } 
  {
    char* tmprc_3098 ;
    int* arrtmp_3572 = (int*)0;
    if (4 > 0) {
      arrtmp_3572 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3572, 4);
    } 
    char* tmpchararr_3571 = (char*)arrtmp_3572;
    memcpy(tmpchararr_3571, "AEG", 4);
     tmprc_3098 = tmpchararr_3571;
     tmpconstlift_1174 = tmprc_3098;
  } 
  {
    char* tmprc_3097 ;
    int* arrtmp_3570 = (int*)0;
    if (5 > 0) {
      arrtmp_3570 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3570, 5);
    } 
    char* tmpchararr_3569 = (char*)arrtmp_3570;
    memcpy(tmpchararr_3569, "AEIS", 5);
     tmprc_3097 = tmpchararr_3569;
     tmpconstlift_1173 = tmprc_3097;
  } 
  {
    char* tmprc_3096 ;
    int* arrtmp_3568 = (int*)0;
    if (4 > 0) {
      arrtmp_3568 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3568, 4);
    } 
    char* tmpchararr_3567 = (char*)arrtmp_3568;
    memcpy(tmpchararr_3567, "AEL", 4);
     tmprc_3096 = tmpchararr_3567;
     tmpconstlift_1172 = tmprc_3096;
  } 
  {
    char* tmprc_3095 ;
    int* arrtmp_3566 = (int*)0;
    if (4 > 0) {
      arrtmp_3566 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3566, 4);
    } 
    char* tmpchararr_3565 = (char*)arrtmp_3566;
    memcpy(tmpchararr_3565, "AEM", 4);
     tmprc_3095 = tmpchararr_3565;
     tmpconstlift_1171 = tmprc_3095;
  } 
  {
    char* tmprc_3094 ;
    int* arrtmp_3564 = (int*)0;
    if (4 > 0) {
      arrtmp_3564 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3564, 4);
    } 
    char* tmpchararr_3563 = (char*)arrtmp_3564;
    memcpy(tmpchararr_3563, "AEO", 4);
     tmprc_3094 = tmpchararr_3563;
     tmpconstlift_1170 = tmprc_3094;
  } 
  {
    char* tmprc_3093 ;
    int* arrtmp_3562 = (int*)0;
    if (4 > 0) {
      arrtmp_3562 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3562, 4);
    } 
    char* tmpchararr_3561 = (char*)arrtmp_3562;
    memcpy(tmpchararr_3561, "AEP", 4);
     tmprc_3093 = tmpchararr_3561;
     tmpconstlift_1169 = tmprc_3093;
  } 
  {
    char* tmprc_3092 ;
    int* arrtmp_3560 = (int*)0;
    if (4 > 0) {
      arrtmp_3560 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3560, 4);
    } 
    char* tmpchararr_3559 = (char*)arrtmp_3560;
    memcpy(tmpchararr_3559, "AER", 4);
     tmprc_3092 = tmpchararr_3559;
     tmpconstlift_1168 = tmprc_3092;
  } 
  {
    char* tmprc_3091 ;
    int* arrtmp_3558 = (int*)0;
    if (4 > 0) {
      arrtmp_3558 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3558, 4);
    } 
    char* tmpchararr_3557 = (char*)arrtmp_3558;
    memcpy(tmpchararr_3557, "AES", 4);
     tmprc_3091 = tmpchararr_3557;
     tmpconstlift_1167 = tmprc_3091;
  } 
  {
    char* tmprc_3090 ;
    int* arrtmp_3556 = (int*)0;
    if (4 > 0) {
      arrtmp_3556 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3556, 4);
    } 
    char* tmpchararr_3555 = (char*)arrtmp_3556;
    memcpy(tmpchararr_3555, "AET", 4);
     tmprc_3090 = tmpchararr_3555;
     tmpconstlift_1166 = tmprc_3090;
  } 
  {
    char* tmprc_3089 ;
    int* arrtmp_3554 = (int*)0;
    if (4 > 0) {
      arrtmp_3554 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3554, 4);
    } 
    char* tmpchararr_3553 = (char*)arrtmp_3554;
    memcpy(tmpchararr_3553, "AEZ", 4);
     tmprc_3089 = tmpchararr_3553;
     tmpconstlift_1165 = tmprc_3089;
  } 
  {
    char* tmprc_3088 ;
    int* arrtmp_3552 = (int*)0;
    if (3 > 0) {
      arrtmp_3552 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 3) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3552, 3);
    } 
    char* tmpchararr_3551 = (char*)arrtmp_3552;
    memcpy(tmpchararr_3551, "AF", 3);
     tmprc_3088 = tmpchararr_3551;
     tmpconstlift_1164 = tmprc_3088;
  } 
  {
    char* tmprc_3087 ;
    int* arrtmp_3550 = (int*)0;
    if (5 > 0) {
      arrtmp_3550 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3550, 5);
    } 
    char* tmpchararr_3549 = (char*)arrtmp_3550;
    memcpy(tmpchararr_3549, "AFAM", 5);
     tmprc_3087 = tmpchararr_3549;
     tmpconstlift_1163 = tmprc_3087;
  } 
  {
    char* tmprc_3086 ;
    int* arrtmp_3548 = (int*)0;
    if (5 > 0) {
      arrtmp_3548 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3548, 5);
    } 
    char* tmpchararr_3547 = (char*)arrtmp_3548;
    memcpy(tmpchararr_3547, "AFFX", 5);
     tmprc_3086 = tmpchararr_3547;
     tmpconstlift_1162 = tmprc_3086;
  } 
  {
    char* tmprc_3085 ;
    int* arrtmp_3546 = (int*)0;
    if (5 > 0) {
      arrtmp_3546 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3546, 5);
    } 
    char* tmpchararr_3545 = (char*)arrtmp_3546;
    memcpy(tmpchararr_3545, "AFFY", 5);
     tmprc_3085 = tmpchararr_3545;
     tmpconstlift_1161 = tmprc_3085;
  } 
  {
    char* tmprc_3084 ;
    int* arrtmp_3544 = (int*)0;
    if (4 > 0) {
      arrtmp_3544 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3544, 4);
    } 
    char* tmpchararr_3543 = (char*)arrtmp_3544;
    memcpy(tmpchararr_3543, "AFG", 4);
     tmprc_3084 = tmpchararr_3543;
     tmpconstlift_1160 = tmprc_3084;
  } 
  {
    char* tmprc_3083 ;
    int* arrtmp_3542 = (int*)0;
    if (4 > 0) {
      arrtmp_3542 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3542, 4);
    } 
    char* tmpchararr_3541 = (char*)arrtmp_3542;
    memcpy(tmpchararr_3541, "AFL", 4);
     tmprc_3083 = tmpchararr_3541;
     tmpconstlift_1159 = tmprc_3083;
  } 
  {
    char* tmprc_3082 ;
    int* arrtmp_3540 = (int*)0;
    if (5 > 0) {
      arrtmp_3540 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3540, 5);
    } 
    char* tmpchararr_3539 = (char*)arrtmp_3540;
    memcpy(tmpchararr_3539, "AFSI", 5);
     tmprc_3082 = tmpchararr_3539;
     tmpconstlift_1158 = tmprc_3082;
  } 
  {
    char* tmprc_3081 ;
    int* arrtmp_3538 = (int*)0;
    if (5 > 0) {
      arrtmp_3538 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3538, 5);
    } 
    char* tmpchararr_3537 = (char*)arrtmp_3538;
    memcpy(tmpchararr_3537, "AGAM", 5);
     tmprc_3081 = tmpchararr_3537;
     tmpconstlift_1157 = tmprc_3081;
  } 
  {
    char* tmprc_3080 ;
    int* arrtmp_3536 = (int*)0;
    if (5 > 0) {
      arrtmp_3536 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3536, 5);
    } 
    char* tmpchararr_3535 = (char*)arrtmp_3536;
    memcpy(tmpchararr_3535, "AGCO", 5);
     tmprc_3080 = tmpchararr_3535;
     tmpconstlift_1156 = tmprc_3080;
  } 
  {
    char* tmprc_3079 ;
    int* arrtmp_3534 = (int*)0;
    if (4 > 0) {
      arrtmp_3534 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3534, 4);
    } 
    char* tmpchararr_3533 = (char*)arrtmp_3534;
    memcpy(tmpchararr_3533, "AGG", 4);
     tmprc_3079 = tmpchararr_3533;
     tmpconstlift_1155 = tmprc_3079;
  } 
  {
    char* tmprc_3078 ;
    int* arrtmp_3532 = (int*)0;
    if (5 > 0) {
      arrtmp_3532 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3532, 5);
    } 
    char* tmpchararr_3531 = (char*)arrtmp_3532;
    memcpy(tmpchararr_3531, "AGII", 5);
     tmprc_3078 = tmpchararr_3531;
     tmpconstlift_1154 = tmprc_3078;
  } 
  {
    char* tmprc_3077 ;
    int* arrtmp_3530 = (int*)0;
    if (4 > 0) {
      arrtmp_3530 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3530, 4);
    } 
    char* tmpchararr_3529 = (char*)arrtmp_3530;
    memcpy(tmpchararr_3529, "AGL", 4);
     tmprc_3077 = tmpchararr_3529;
     tmpconstlift_1153 = tmprc_3077;
  } 
  {
    char* tmprc_3076 ;
    int* arrtmp_3528 = (int*)0;
    if (4 > 0) {
      arrtmp_3528 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3528, 4);
    } 
    char* tmpchararr_3527 = (char*)arrtmp_3528;
    memcpy(tmpchararr_3527, "AGM", 4);
     tmprc_3076 = tmpchararr_3527;
     tmpconstlift_1152 = tmprc_3076;
  } 
  {
    char* tmprc_3075 ;
    int* arrtmp_3526 = (int*)0;
    if (4 > 0) {
      arrtmp_3526 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3526, 4);
    } 
    char* tmpchararr_3525 = (char*)arrtmp_3526;
    memcpy(tmpchararr_3525, "AGN", 4);
     tmprc_3075 = tmpchararr_3525;
     tmpconstlift_1151 = tmprc_3075;
  } 
  {
    char* tmprc_3074 ;
    int* arrtmp_3524 = (int*)0;
    if (5 > 0) {
      arrtmp_3524 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3524, 5);
    } 
    char* tmpchararr_3523 = (char*)arrtmp_3524;
    memcpy(tmpchararr_3523, "AGNC", 5);
     tmprc_3074 = tmpchararr_3523;
     tmpconstlift_1150 = tmprc_3074;
  } 
  {
    char* tmprc_3073 ;
    int* arrtmp_3522 = (int*)0;
    if (4 > 0) {
      arrtmp_3522 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3522, 4);
    } 
    char* tmpchararr_3521 = (char*)arrtmp_3522;
    memcpy(tmpchararr_3521, "AGO", 4);
     tmprc_3073 = tmpchararr_3521;
     tmpconstlift_1149 = tmprc_3073;
  } 
  {
    char* tmprc_3072 ;
    int* arrtmp_3520 = (int*)0;
    if (4 > 0) {
      arrtmp_3520 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3520, 4);
    } 
    char* tmpchararr_3519 = (char*)arrtmp_3520;
    memcpy(tmpchararr_3519, "AGP", 4);
     tmprc_3072 = tmpchararr_3519;
     tmpconstlift_1148 = tmprc_3072;
  } 
  {
    char* tmprc_3071 ;
    int* arrtmp_3518 = (int*)0;
    if (4 > 0) {
      arrtmp_3518 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3518, 4);
    } 
    char* tmpchararr_3517 = (char*)arrtmp_3518;
    memcpy(tmpchararr_3517, "AGQ", 4);
     tmprc_3071 = tmpchararr_3517;
     tmpconstlift_1147 = tmprc_3071;
  } 
  {
    char* tmprc_3070 ;
    int* arrtmp_3516 = (int*)0;
    if (4 > 0) {
      arrtmp_3516 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3516, 4);
    } 
    char* tmpchararr_3515 = (char*)arrtmp_3516;
    memcpy(tmpchararr_3515, "AGU", 4);
     tmprc_3070 = tmpchararr_3515;
     tmpconstlift_1146 = tmprc_3070;
  } 
  {
    char* tmprc_3069 ;
    int* arrtmp_3514 = (int*)0;
    if (5 > 0) {
      arrtmp_3514 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3514, 5);
    } 
    char* tmpchararr_3513 = (char*)arrtmp_3514;
    memcpy(tmpchararr_3513, "AGYS", 5);
     tmprc_3069 = tmpchararr_3513;
     tmpconstlift_1145 = tmprc_3069;
  } 
  {
    char* tmprc_3068 ;
    int* arrtmp_3512 = (int*)0;
    if (5 > 0) {
      arrtmp_3512 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3512, 5);
    } 
    char* tmpchararr_3511 = (char*)arrtmp_3512;
    memcpy(tmpchararr_3511, "AHGP", 5);
     tmprc_3068 = tmpchararr_3511;
     tmpconstlift_1144 = tmprc_3068;
  } 
  {
    char* tmprc_3067 ;
    int* arrtmp_3510 = (int*)0;
    if (4 > 0) {
      arrtmp_3510 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3510, 4);
    } 
    char* tmpchararr_3509 = (char*)arrtmp_3510;
    memcpy(tmpchararr_3509, "AHL", 4);
     tmprc_3067 = tmpchararr_3509;
     tmpconstlift_1143 = tmprc_3067;
  } 
  {
    char* tmprc_3066 ;
    int* arrtmp_3508 = (int*)0;
    if (4 > 0) {
      arrtmp_3508 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3508, 4);
    } 
    char* tmpchararr_3507 = (char*)arrtmp_3508;
    memcpy(tmpchararr_3507, "AHS", 4);
     tmprc_3066 = tmpchararr_3507;
     tmpconstlift_1142 = tmprc_3066;
  } 
  {
    char* tmprc_3065 ;
    int* arrtmp_3506 = (int*)0;
    if (4 > 0) {
      arrtmp_3506 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3506, 4);
    } 
    char* tmpchararr_3505 = (char*)arrtmp_3506;
    memcpy(tmpchararr_3505, "AHT", 4);
     tmprc_3065 = tmpchararr_3505;
     tmpconstlift_1141 = tmprc_3065;
  } 
  {
    char* tmprc_3064 ;
    int* arrtmp_3504 = (int*)0;
    if (4 > 0) {
      arrtmp_3504 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3504, 4);
    } 
    char* tmpchararr_3503 = (char*)arrtmp_3504;
    memcpy(tmpchararr_3503, "AIG", 4);
     tmprc_3064 = tmpchararr_3503;
     tmpconstlift_1140 = tmprc_3064;
  } 
  {
    char* tmprc_3063 ;
    int* arrtmp_3502 = (int*)0;
    if (5 > 0) {
      arrtmp_3502 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3502, 5);
    } 
    char* tmpchararr_3501 = (char*)arrtmp_3502;
    memcpy(tmpchararr_3501, "AIMC", 5);
     tmprc_3063 = tmpchararr_3501;
     tmpconstlift_1139 = tmprc_3063;
  } 
  {
    char* tmprc_3062 ;
    int* arrtmp_3500 = (int*)0;
    if (4 > 0) {
      arrtmp_3500 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3500, 4);
    } 
    char* tmpchararr_3499 = (char*)arrtmp_3500;
    memcpy(tmpchararr_3499, "AIN", 4);
     tmprc_3062 = tmpchararr_3499;
     tmpconstlift_1138 = tmprc_3062;
  } 
  {
    char* tmprc_3061 ;
    int* arrtmp_3498 = (int*)0;
    if (5 > 0) {
      arrtmp_3498 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3498, 5);
    } 
    char* tmpchararr_3497 = (char*)arrtmp_3498;
    memcpy(tmpchararr_3497, "AINV", 5);
     tmprc_3061 = tmpchararr_3497;
     tmpconstlift_1137 = tmprc_3061;
  } 
  {
    char* tmprc_3060 ;
    int* arrtmp_3496 = (int*)0;
    if (5 > 0) {
      arrtmp_3496 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3496, 5);
    } 
    char* tmpchararr_3495 = (char*)arrtmp_3496;
    memcpy(tmpchararr_3495, "AIPC", 5);
     tmprc_3060 = tmpchararr_3495;
     tmpconstlift_1136 = tmprc_3060;
  } 
  {
    char* tmprc_3059 ;
    int* arrtmp_3494 = (int*)0;
    if (4 > 0) {
      arrtmp_3494 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3494, 4);
    } 
    char* tmpchararr_3493 = (char*)arrtmp_3494;
    memcpy(tmpchararr_3493, "AIR", 4);
     tmprc_3059 = tmpchararr_3493;
     tmpconstlift_1135 = tmprc_3059;
  } 
  {
    char* tmprc_3058 ;
    int* arrtmp_3492 = (int*)0;
    if (5 > 0) {
      arrtmp_3492 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3492, 5);
    } 
    char* tmpchararr_3491 = (char*)arrtmp_3492;
    memcpy(tmpchararr_3491, "AIRM", 5);
     tmprc_3058 = tmpchararr_3491;
     tmpconstlift_1134 = tmprc_3058;
  } 
  {
    char* tmprc_3057 ;
    int* arrtmp_3490 = (int*)0;
    if (4 > 0) {
      arrtmp_3490 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3490, 4);
    } 
    char* tmpchararr_3489 = (char*)arrtmp_3490;
    memcpy(tmpchararr_3489, "AIT", 4);
     tmprc_3057 = tmpchararr_3489;
     tmpconstlift_1133 = tmprc_3057;
  } 
  {
    char* tmprc_3056 ;
    int* arrtmp_3488 = (int*)0;
    if (4 > 0) {
      arrtmp_3488 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3488, 4);
    } 
    char* tmpchararr_3487 = (char*)arrtmp_3488;
    memcpy(tmpchararr_3487, "AIV", 4);
     tmprc_3056 = tmpchararr_3487;
     tmpconstlift_1132 = tmprc_3056;
  } 
  {
    char* tmprc_3055 ;
    int* arrtmp_3486 = (int*)0;
    if (5 > 0) {
      arrtmp_3486 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3486, 5);
    } 
    char* tmpchararr_3485 = (char*)arrtmp_3486;
    memcpy(tmpchararr_3485, "AIXG", 5);
     tmprc_3055 = tmpchararr_3485;
     tmpconstlift_1131 = tmprc_3055;
  } 
  {
    char* tmprc_3054 ;
    int* arrtmp_3484 = (int*)0;
    if (4 > 0) {
      arrtmp_3484 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3484, 4);
    } 
    char* tmpchararr_3483 = (char*)arrtmp_3484;
    memcpy(tmpchararr_3483, "AIZ", 4);
     tmprc_3054 = tmpchararr_3483;
     tmpconstlift_1130 = tmprc_3054;
  } 
  {
    char* tmprc_3053 ;
    int* arrtmp_3482 = (int*)0;
    if (4 > 0) {
      arrtmp_3482 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3482, 4);
    } 
    char* tmpchararr_3481 = (char*)arrtmp_3482;
    memcpy(tmpchararr_3481, "AJG", 4);
     tmprc_3053 = tmpchararr_3481;
     tmpconstlift_1129 = tmprc_3053;
  } 
  {
    char* tmprc_3052 ;
    int* arrtmp_3480 = (int*)0;
    if (5 > 0) {
      arrtmp_3480 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3480, 5);
    } 
    char* tmpchararr_3479 = (char*)arrtmp_3480;
    memcpy(tmpchararr_3479, "AKAM", 5);
     tmprc_3052 = tmpchararr_3479;
     tmpconstlift_1128 = tmprc_3052;
  } 
  {
    char* tmprc_3051 ;
    int* arrtmp_3478 = (int*)0;
    if (4 > 0) {
      arrtmp_3478 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3478, 4);
    } 
    char* tmpchararr_3477 = (char*)arrtmp_3478;
    memcpy(tmpchararr_3477, "AKR", 4);
     tmprc_3051 = tmpchararr_3477;
     tmpconstlift_1127 = tmprc_3051;
  } 
  {
    char* tmprc_3050 ;
    int* arrtmp_3476 = (int*)0;
    if (4 > 0) {
      arrtmp_3476 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3476, 4);
    } 
    char* tmpchararr_3475 = (char*)arrtmp_3476;
    memcpy(tmpchararr_3475, "AKS", 4);
     tmprc_3050 = tmpchararr_3475;
     tmpconstlift_1126 = tmprc_3050;
  } 
  {
    char* tmprc_3049 ;
    int* arrtmp_3474 = (int*)0;
    if (4 > 0) {
      arrtmp_3474 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3474, 4);
    } 
    char* tmpchararr_3473 = (char*)arrtmp_3474;
    memcpy(tmpchararr_3473, "ALB", 4);
     tmprc_3049 = tmpchararr_3473;
     tmpconstlift_1125 = tmprc_3049;
  } 
  {
    char* tmprc_3048 ;
    int* arrtmp_3472 = (int*)0;
    if (4 > 0) {
      arrtmp_3472 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3472, 4);
    } 
    char* tmpchararr_3471 = (char*)arrtmp_3472;
    memcpy(tmpchararr_3471, "ALE", 4);
     tmprc_3048 = tmpchararr_3471;
     tmpconstlift_1124 = tmprc_3048;
  } 
  {
    char* tmprc_3047 ;
    int* arrtmp_3470 = (int*)0;
    if (5 > 0) {
      arrtmp_3470 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3470, 5);
    } 
    char* tmpchararr_3469 = (char*)arrtmp_3470;
    memcpy(tmpchararr_3469, "ALEX", 5);
     tmprc_3047 = tmpchararr_3469;
     tmpconstlift_1123 = tmprc_3047;
  } 
  {
    char* tmprc_3046 ;
    int* arrtmp_3468 = (int*)0;
    if (5 > 0) {
      arrtmp_3468 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3468, 5);
    } 
    char* tmpchararr_3467 = (char*)arrtmp_3468;
    memcpy(tmpchararr_3467, "ALGN", 5);
     tmprc_3046 = tmpchararr_3467;
     tmpconstlift_1122 = tmprc_3046;
  } 
  {
    char* tmprc_3045 ;
    int* arrtmp_3466 = (int*)0;
    if (5 > 0) {
      arrtmp_3466 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3466, 5);
    } 
    char* tmpchararr_3465 = (char*)arrtmp_3466;
    memcpy(tmpchararr_3465, "ALGT", 5);
     tmprc_3045 = tmpchararr_3465;
     tmpconstlift_1121 = tmprc_3045;
  } 
  {
    char* tmprc_3044 ;
    int* arrtmp_3464 = (int*)0;
    if (4 > 0) {
      arrtmp_3464 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3464, 4);
    } 
    char* tmpchararr_3463 = (char*)arrtmp_3464;
    memcpy(tmpchararr_3463, "ALJ", 4);
     tmprc_3044 = tmpchararr_3463;
     tmpconstlift_1120 = tmprc_3044;
  } 
  {
    char* tmprc_3043 ;
    int* arrtmp_3462 = (int*)0;
    if (4 > 0) {
      arrtmp_3462 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3462, 4);
    } 
    char* tmpchararr_3461 = (char*)arrtmp_3462;
    memcpy(tmpchararr_3461, "ALK", 4);
     tmprc_3043 = tmpchararr_3461;
     tmpconstlift_1119 = tmprc_3043;
  } 
  {
    char* tmprc_3042 ;
    int* arrtmp_3460 = (int*)0;
    if (5 > 0) {
      arrtmp_3460 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3460, 5);
    } 
    char* tmpchararr_3459 = (char*)arrtmp_3460;
    memcpy(tmpchararr_3459, "ALKS", 5);
     tmprc_3042 = tmpchararr_3459;
     tmpconstlift_1118 = tmprc_3042;
  } 
  {
    char* tmprc_3041 ;
    int* arrtmp_3458 = (int*)0;
    if (4 > 0) {
      arrtmp_3458 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3458, 4);
    } 
    char* tmpchararr_3457 = (char*)arrtmp_3458;
    memcpy(tmpchararr_3457, "ALL", 4);
     tmprc_3041 = tmpchararr_3457;
     tmpconstlift_1117 = tmprc_3041;
  } 
  {
    char* tmprc_3040 ;
    int* arrtmp_3456 = (int*)0;
    if (5 > 0) {
      arrtmp_3456 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3456, 5);
    } 
    char* tmpchararr_3455 = (char*)arrtmp_3456;
    memcpy(tmpchararr_3455, "ALNY", 5);
     tmprc_3040 = tmpchararr_3455;
     tmpconstlift_1116 = tmprc_3040;
  } 
  {
    char* tmprc_3039 ;
    int* arrtmp_3454 = (int*)0;
    if (5 > 0) {
      arrtmp_3454 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3454, 5);
    } 
    char* tmpchararr_3453 = (char*)arrtmp_3454;
    memcpy(tmpchararr_3453, "ALOG", 5);
     tmprc_3039 = tmpchararr_3453;
     tmpconstlift_1115 = tmprc_3039;
  } 
  {
    char* tmprc_3038 ;
    int* arrtmp_3452 = (int*)0;
    if (5 > 0) {
      arrtmp_3452 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3452, 5);
    } 
    char* tmpchararr_3451 = (char*)arrtmp_3452;
    memcpy(tmpchararr_3451, "ALSK", 5);
     tmprc_3038 = tmpchararr_3451;
     tmpconstlift_1114 = tmprc_3038;
  } 
  {
    char* tmprc_3037 ;
    int* arrtmp_3450 = (int*)0;
    if (5 > 0) {
      arrtmp_3450 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3450, 5);
    } 
    char* tmpchararr_3449 = (char*)arrtmp_3450;
    memcpy(tmpchararr_3449, "ALTE", 5);
     tmprc_3037 = tmpchararr_3449;
     tmpconstlift_1113 = tmprc_3037;
  } 
  {
    char* tmprc_3036 ;
    int* arrtmp_3448 = (int*)0;
    if (5 > 0) {
      arrtmp_3448 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3448, 5);
    } 
    char* tmpchararr_3447 = (char*)arrtmp_3448;
    memcpy(tmpchararr_3447, "ALTH", 5);
     tmprc_3036 = tmpchararr_3447;
     tmpconstlift_1112 = tmprc_3036;
  } 
  {
    char* tmprc_3035 ;
    int* arrtmp_3446 = (int*)0;
    if (5 > 0) {
      arrtmp_3446 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3446, 5);
    } 
    char* tmpchararr_3445 = (char*)arrtmp_3446;
    memcpy(tmpchararr_3445, "ALTR", 5);
     tmprc_3035 = tmpchararr_3445;
     tmpconstlift_1111 = tmprc_3035;
  } 
  {
    char* tmprc_3034 ;
    int* arrtmp_3444 = (int*)0;
    if (4 > 0) {
      arrtmp_3444 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3444, 4);
    } 
    char* tmpchararr_3443 = (char*)arrtmp_3444;
    memcpy(tmpchararr_3443, "ALV", 4);
     tmprc_3034 = tmpchararr_3443;
     tmpconstlift_1110 = tmprc_3034;
  } 
  {
    char* tmprc_3033 ;
    int* arrtmp_3442 = (int*)0;
    if (5 > 0) {
      arrtmp_3442 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3442, 5);
    } 
    char* tmpchararr_3441 = (char*)arrtmp_3442;
    memcpy(tmpchararr_3441, "ALXN", 5);
     tmprc_3033 = tmpchararr_3441;
     tmpconstlift_1109 = tmprc_3033;
  } 
  {
    char* tmprc_3032 ;
    int* arrtmp_3440 = (int*)0;
    if (3 > 0) {
      arrtmp_3440 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 3) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3440, 3);
    } 
    char* tmpchararr_3439 = (char*)arrtmp_3440;
    memcpy(tmpchararr_3439, "AM", 3);
     tmprc_3032 = tmpchararr_3439;
     tmpconstlift_1108 = tmprc_3032;
  } 
  {
    char* tmprc_3031 ;
    int* arrtmp_3438 = (int*)0;
    if (5 > 0) {
      arrtmp_3438 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3438, 5);
    } 
    char* tmpchararr_3437 = (char*)arrtmp_3438;
    memcpy(tmpchararr_3437, "AMAG", 5);
     tmprc_3031 = tmpchararr_3437;
     tmpconstlift_1107 = tmprc_3031;
  } 
  {
    char* tmprc_3030 ;
    int* arrtmp_3436 = (int*)0;
    if (5 > 0) {
      arrtmp_3436 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3436, 5);
    } 
    char* tmpchararr_3435 = (char*)arrtmp_3436;
    memcpy(tmpchararr_3435, "AMAT", 5);
     tmprc_3030 = tmpchararr_3435;
     tmpconstlift_1106 = tmprc_3030;
  } 
  {
    char* tmprc_3029 ;
    int* arrtmp_3434 = (int*)0;
    if (4 > 0) {
      arrtmp_3434 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3434, 4);
    } 
    char* tmpchararr_3433 = (char*)arrtmp_3434;
    memcpy(tmpchararr_3433, "AMB", 4);
     tmprc_3029 = tmpchararr_3433;
     tmpconstlift_1105 = tmprc_3029;
  } 
  {
    char* tmprc_3028 ;
    int* arrtmp_3432 = (int*)0;
    if (5 > 0) {
      arrtmp_3432 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3432, 5);
    } 
    char* tmpchararr_3431 = (char*)arrtmp_3432;
    memcpy(tmpchararr_3431, "AMCC", 5);
     tmprc_3028 = tmpchararr_3431;
     tmpconstlift_1104 = tmprc_3028;
  } 
  {
    char* tmprc_3027 ;
    int* arrtmp_3430 = (int*)0;
    if (4 > 0) {
      arrtmp_3430 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3430, 4);
    } 
    char* tmpchararr_3429 = (char*)arrtmp_3430;
    memcpy(tmpchararr_3429, "AMD", 4);
     tmprc_3027 = tmpchararr_3429;
     tmpconstlift_1103 = tmprc_3027;
  } 
  {
    char* tmprc_3026 ;
    int* arrtmp_3428 = (int*)0;
    if (4 > 0) {
      arrtmp_3428 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3428, 4);
    } 
    char* tmpchararr_3427 = (char*)arrtmp_3428;
    memcpy(tmpchararr_3427, "AME", 4);
     tmprc_3026 = tmpchararr_3427;
     tmpconstlift_1102 = tmprc_3026;
  } 
  {
    char* tmprc_3025 ;
    int* arrtmp_3426 = (int*)0;
    if (5 > 0) {
      arrtmp_3426 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3426, 5);
    } 
    char* tmpchararr_3425 = (char*)arrtmp_3426;
    memcpy(tmpchararr_3425, "AMED", 5);
     tmprc_3025 = tmpchararr_3425;
     tmpconstlift_1101 = tmprc_3025;
  } 
  {
    char* tmprc_3024 ;
    int* arrtmp_3424 = (int*)0;
    if (4 > 0) {
      arrtmp_3424 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3424, 4);
    } 
    char* tmpchararr_3423 = (char*)arrtmp_3424;
    memcpy(tmpchararr_3423, "AMG", 4);
     tmprc_3024 = tmpchararr_3423;
     tmpconstlift_1100 = tmprc_3024;
  } 
  {
    char* tmprc_3023 ;
    int* arrtmp_3422 = (int*)0;
    if (5 > 0) {
      arrtmp_3422 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3422, 5);
    } 
    char* tmpchararr_3421 = (char*)arrtmp_3422;
    memcpy(tmpchararr_3421, "AMGN", 5);
     tmprc_3023 = tmpchararr_3421;
     tmpconstlift_1099 = tmprc_3023;
  } 
  {
    char* tmprc_3022 ;
    int* arrtmp_3420 = (int*)0;
    if (4 > 0) {
      arrtmp_3420 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3420, 4);
    } 
    char* tmpchararr_3419 = (char*)arrtmp_3420;
    memcpy(tmpchararr_3419, "AMJ", 4);
     tmprc_3022 = tmpchararr_3419;
     tmpconstlift_1098 = tmprc_3022;
  } 
  {
    char* tmprc_3021 ;
    int* arrtmp_3418 = (int*)0;
    if (5 > 0) {
      arrtmp_3418 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3418, 5);
    } 
    char* tmpchararr_3417 = (char*)arrtmp_3418;
    memcpy(tmpchararr_3417, "AMKR", 5);
     tmprc_3021 = tmpchararr_3417;
     tmpconstlift_1097 = tmprc_3021;
  } 
  {
    char* tmprc_3020 ;
    int* arrtmp_3416 = (int*)0;
    if (5 > 0) {
      arrtmp_3416 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3416, 5);
    } 
    char* tmpchararr_3415 = (char*)arrtmp_3416;
    memcpy(tmpchararr_3415, "AMLN", 5);
     tmprc_3020 = tmpchararr_3415;
     tmpconstlift_1096 = tmprc_3020;
  } 
  {
    char* tmprc_3019 ;
    int* arrtmp_3414 = (int*)0;
    if (5 > 0) {
      arrtmp_3414 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3414, 5);
    } 
    char* tmpchararr_3413 = (char*)arrtmp_3414;
    memcpy(tmpchararr_3413, "AMMD", 5);
     tmprc_3019 = tmpchararr_3413;
     tmpconstlift_1095 = tmprc_3019;
  } 
  {
    char* tmprc_3018 ;
    int* arrtmp_3412 = (int*)0;
    if (4 > 0) {
      arrtmp_3412 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3412, 4);
    } 
    char* tmpchararr_3411 = (char*)arrtmp_3412;
    memcpy(tmpchararr_3411, "AMN", 4);
     tmprc_3018 = tmpchararr_3411;
     tmpconstlift_1094 = tmprc_3018;
  } 
  {
    char* tmprc_3017 ;
    int* arrtmp_3410 = (int*)0;
    if (4 > 0) {
      arrtmp_3410 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3410, 4);
    } 
    char* tmpchararr_3409 = (char*)arrtmp_3410;
    memcpy(tmpchararr_3409, "AMP", 4);
     tmprc_3017 = tmpchararr_3409;
     tmpconstlift_1093 = tmprc_3017;
  } 
  {
    char* tmprc_3016 ;
    int* arrtmp_3408 = (int*)0;
    if (4 > 0) {
      arrtmp_3408 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3408, 4);
    } 
    char* tmpchararr_3407 = (char*)arrtmp_3408;
    memcpy(tmpchararr_3407, "AMR", 4);
     tmprc_3016 = tmpchararr_3407;
     tmpconstlift_1092 = tmprc_3016;
  } 
  {
    char* tmprc_3015 ;
    int* arrtmp_3406 = (int*)0;
    if (5 > 0) {
      arrtmp_3406 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3406, 5);
    } 
    char* tmpchararr_3405 = (char*)arrtmp_3406;
    memcpy(tmpchararr_3405, "AMRI", 5);
     tmprc_3015 = tmpchararr_3405;
     tmpconstlift_1091 = tmprc_3015;
  } 
  {
    char* tmprc_3014 ;
    int* arrtmp_3404 = (int*)0;
    if (5 > 0) {
      arrtmp_3404 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3404, 5);
    } 
    char* tmpchararr_3403 = (char*)arrtmp_3404;
    memcpy(tmpchararr_3403, "AMSC", 5);
     tmprc_3014 = tmpchararr_3403;
     tmpconstlift_1090 = tmprc_3014;
  } 
  {
    char* tmprc_3013 ;
    int* arrtmp_3402 = (int*)0;
    if (5 > 0) {
      arrtmp_3402 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3402, 5);
    } 
    char* tmpchararr_3401 = (char*)arrtmp_3402;
    memcpy(tmpchararr_3401, "AMSF", 5);
     tmprc_3013 = tmpchararr_3401;
     tmpconstlift_1089 = tmprc_3013;
  } 
  {
    char* tmprc_3012 ;
    int* arrtmp_3400 = (int*)0;
    if (5 > 0) {
      arrtmp_3400 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3400, 5);
    } 
    char* tmpchararr_3399 = (char*)arrtmp_3400;
    memcpy(tmpchararr_3399, "AMSG", 5);
     tmprc_3012 = tmpchararr_3399;
     tmpconstlift_1088 = tmprc_3012;
  } 
  {
    char* tmprc_3011 ;
    int* arrtmp_3398 = (int*)0;
    if (4 > 0) {
      arrtmp_3398 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3398, 4);
    } 
    char* tmpchararr_3397 = (char*)arrtmp_3398;
    memcpy(tmpchararr_3397, "AMT", 4);
     tmprc_3011 = tmpchararr_3397;
     tmpconstlift_1087 = tmprc_3011;
  } 
  {
    char* tmprc_3010 ;
    int* arrtmp_3396 = (int*)0;
    if (5 > 0) {
      arrtmp_3396 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3396, 5);
    } 
    char* tmpchararr_3395 = (char*)arrtmp_3396;
    memcpy(tmpchararr_3395, "AMTD", 5);
     tmprc_3010 = tmpchararr_3395;
     tmpconstlift_1086 = tmprc_3010;
  } 
  {
    char* tmprc_3009 ;
    int* arrtmp_3394 = (int*)0;
    if (4 > 0) {
      arrtmp_3394 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3394, 4);
    } 
    char* tmpchararr_3393 = (char*)arrtmp_3394;
    memcpy(tmpchararr_3393, "AMX", 4);
     tmprc_3009 = tmpchararr_3393;
     tmpconstlift_1085 = tmprc_3009;
  } 
  {
    char* tmprc_3008 ;
    int* arrtmp_3392 = (int*)0;
    if (5 > 0) {
      arrtmp_3392 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3392, 5);
    } 
    char* tmpchararr_3391 = (char*)arrtmp_3392;
    memcpy(tmpchararr_3391, "AMZN", 5);
     tmprc_3008 = tmpchararr_3391;
     tmpconstlift_1084 = tmprc_3008;
  } 
  {
    char* tmprc_3007 ;
    int* arrtmp_3390 = (int*)0;
    if (3 > 0) {
      arrtmp_3390 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 3) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3390, 3);
    } 
    char* tmpchararr_3389 = (char*)arrtmp_3390;
    memcpy(tmpchararr_3389, "AN", 3);
     tmprc_3007 = tmpchararr_3389;
     tmpconstlift_1083 = tmprc_3007;
  } 
  {
    char* tmprc_3006 ;
    int* arrtmp_3388 = (int*)0;
    if (5 > 0) {
      arrtmp_3388 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3388, 5);
    } 
    char* tmpchararr_3387 = (char*)arrtmp_3388;
    memcpy(tmpchararr_3387, "ANDE", 5);
     tmprc_3006 = tmpchararr_3387;
     tmpconstlift_1082 = tmprc_3006;
  } 
  {
    char* tmprc_3005 ;
    int* arrtmp_3386 = (int*)0;
    if (4 > 0) {
      arrtmp_3386 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3386, 4);
    } 
    char* tmpchararr_3385 = (char*)arrtmp_3386;
    memcpy(tmpchararr_3385, "ANF", 4);
     tmprc_3005 = tmpchararr_3385;
     tmpconstlift_1081 = tmprc_3005;
  } 
  {
    char* tmprc_3004 ;
    int* arrtmp_3384 = (int*)0;
    if (5 > 0) {
      arrtmp_3384 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3384, 5);
    } 
    char* tmpchararr_3383 = (char*)arrtmp_3384;
    memcpy(tmpchararr_3383, "ANGO", 5);
     tmprc_3004 = tmpchararr_3383;
     tmpconstlift_1080 = tmprc_3004;
  } 
  {
    char* tmprc_3003 ;
    int* arrtmp_3382 = (int*)0;
    if (4 > 0) {
      arrtmp_3382 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3382, 4);
    } 
    char* tmpchararr_3381 = (char*)arrtmp_3382;
    memcpy(tmpchararr_3381, "ANH", 4);
     tmprc_3003 = tmpchararr_3381;
     tmpconstlift_1079 = tmprc_3003;
  } 
  {
    char* tmprc_3002 ;
    int* arrtmp_3380 = (int*)0;
    if (4 > 0) {
      arrtmp_3380 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3380, 4);
    } 
    char* tmpchararr_3379 = (char*)arrtmp_3380;
    memcpy(tmpchararr_3379, "ANN", 4);
     tmprc_3002 = tmpchararr_3379;
     tmpconstlift_1078 = tmprc_3002;
  } 
  {
    char* tmprc_3001 ;
    int* arrtmp_3378 = (int*)0;
    if (4 > 0) {
      arrtmp_3378 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3378, 4);
    } 
    char* tmpchararr_3377 = (char*)arrtmp_3378;
    memcpy(tmpchararr_3377, "ANR", 4);
     tmprc_3001 = tmpchararr_3377;
     tmpconstlift_1077 = tmprc_3001;
  } 
  {
    char* tmprc_3000 ;
    int* arrtmp_3376 = (int*)0;
    if (5 > 0) {
      arrtmp_3376 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3376, 5);
    } 
    char* tmpchararr_3375 = (char*)arrtmp_3376;
    memcpy(tmpchararr_3375, "ANSS", 5);
     tmprc_3000 = tmpchararr_3375;
     tmpconstlift_1076 = tmprc_3000;
  } 
  {
    char* tmprc_2999 ;
    int* arrtmp_3374 = (int*)0;
    if (4 > 0) {
      arrtmp_3374 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3374, 4);
    } 
    char* tmpchararr_3373 = (char*)arrtmp_3374;
    memcpy(tmpchararr_3373, "ANV", 4);
     tmprc_2999 = tmpchararr_3373;
     tmpconstlift_1075 = tmprc_2999;
  } 
  {
    char* tmprc_2998 ;
    int* arrtmp_3372 = (int*)0;
    if (4 > 0) {
      arrtmp_3372 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3372, 4);
    } 
    char* tmpchararr_3371 = (char*)arrtmp_3372;
    memcpy(tmpchararr_3371, "ANW", 4);
     tmprc_2998 = tmpchararr_3371;
     tmpconstlift_1074 = tmprc_2998;
  } 
  {
    char* tmprc_2997 ;
    int* arrtmp_3370 = (int*)0;
    if (4 > 0) {
      arrtmp_3370 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3370, 4);
    } 
    char* tmpchararr_3369 = (char*)arrtmp_3370;
    memcpy(tmpchararr_3369, "AOL", 4);
     tmprc_2997 = tmpchararr_3369;
     tmpconstlift_1073 = tmprc_2997;
  } 
  {
    char* tmprc_2996 ;
    int* arrtmp_3368 = (int*)0;
    if (4 > 0) {
      arrtmp_3368 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3368, 4);
    } 
    char* tmpchararr_3367 = (char*)arrtmp_3368;
    memcpy(tmpchararr_3367, "AON", 4);
     tmprc_2996 = tmpchararr_3367;
     tmpconstlift_1072 = tmprc_2996;
  } 
  {
    char* tmprc_2995 ;
    int* arrtmp_3366 = (int*)0;
    if (5 > 0) {
      arrtmp_3366 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3366, 5);
    } 
    char* tmpchararr_3365 = (char*)arrtmp_3366;
    memcpy(tmpchararr_3365, "AONE", 5);
     tmprc_2995 = tmpchararr_3365;
     tmpconstlift_1071 = tmprc_2995;
  } 
  {
    char* tmprc_2994 ;
    int* arrtmp_3364 = (int*)0;
    if (4 > 0) {
      arrtmp_3364 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3364, 4);
    } 
    char* tmpchararr_3363 = (char*)arrtmp_3364;
    memcpy(tmpchararr_3363, "AOS", 4);
     tmprc_2994 = tmpchararr_3363;
     tmpconstlift_1070 = tmprc_2994;
  } 
  {
    char* tmprc_2993 ;
    int* arrtmp_3362 = (int*)0;
    if (4 > 0) {
      arrtmp_3362 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3362, 4);
    } 
    char* tmpchararr_3361 = (char*)arrtmp_3362;
    memcpy(tmpchararr_3361, "APA", 4);
     tmprc_2993 = tmpchararr_3361;
     tmpconstlift_1069 = tmprc_2993;
  } 
  {
    char* tmprc_2992 ;
    int* arrtmp_3360 = (int*)0;
    if (5 > 0) {
      arrtmp_3360 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3360, 5);
    } 
    char* tmpchararr_3359 = (char*)arrtmp_3360;
    memcpy(tmpchararr_3359, "APAC", 5);
     tmprc_2992 = tmpchararr_3359;
     tmpconstlift_1068 = tmprc_2992;
  } 
  {
    char* tmprc_2991 ;
    int* arrtmp_3358 = (int*)0;
    if (4 > 0) {
      arrtmp_3358 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3358, 4);
    } 
    char* tmpchararr_3357 = (char*)arrtmp_3358;
    memcpy(tmpchararr_3357, "APC", 4);
     tmprc_2991 = tmpchararr_3357;
     tmpconstlift_1067 = tmprc_2991;
  } 
  {
    char* tmprc_2990 ;
    int* arrtmp_3356 = (int*)0;
    if (4 > 0) {
      arrtmp_3356 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3356, 4);
    } 
    char* tmpchararr_3355 = (char*)arrtmp_3356;
    memcpy(tmpchararr_3355, "APD", 4);
     tmprc_2990 = tmpchararr_3355;
     tmpconstlift_1066 = tmprc_2990;
  } 
  {
    char* tmprc_2989 ;
    int* arrtmp_3354 = (int*)0;
    if (5 > 0) {
      arrtmp_3354 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3354, 5);
    } 
    char* tmpchararr_3353 = (char*)arrtmp_3354;
    memcpy(tmpchararr_3353, "APEI", 5);
     tmprc_2989 = tmpchararr_3353;
     tmpconstlift_1065 = tmprc_2989;
  } 
  {
    char* tmprc_2988 ;
    int* arrtmp_3352 = (int*)0;
    if (4 > 0) {
      arrtmp_3352 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3352, 4);
    } 
    char* tmpchararr_3351 = (char*)arrtmp_3352;
    memcpy(tmpchararr_3351, "APH", 4);
     tmprc_2988 = tmpchararr_3351;
     tmpconstlift_1064 = tmprc_2988;
  } 
  {
    char* tmprc_2987 ;
    int* arrtmp_3350 = (int*)0;
    if (5 > 0) {
      arrtmp_3350 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3350, 5);
    } 
    char* tmpchararr_3349 = (char*)arrtmp_3350;
    memcpy(tmpchararr_3349, "APKT", 5);
     tmprc_2987 = tmpchararr_3349;
     tmpconstlift_1063 = tmprc_2987;
  } 
  {
    char* tmprc_2986 ;
    int* arrtmp_3348 = (int*)0;
    if (4 > 0) {
      arrtmp_3348 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3348, 4);
    } 
    char* tmpchararr_3347 = (char*)arrtmp_3348;
    memcpy(tmpchararr_3347, "APL", 4);
     tmprc_2986 = tmpchararr_3347;
     tmpconstlift_1062 = tmprc_2986;
  } 
  {
    char* tmprc_2985 ;
    int* arrtmp_3346 = (int*)0;
    if (5 > 0) {
      arrtmp_3346 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3346, 5);
    } 
    char* tmpchararr_3345 = (char*)arrtmp_3346;
    memcpy(tmpchararr_3345, "APOG", 5);
     tmprc_2985 = tmpchararr_3345;
     tmpconstlift_1061 = tmprc_2985;
  } 
  {
    char* tmprc_2984 ;
    int* arrtmp_3344 = (int*)0;
    if (5 > 0) {
      arrtmp_3344 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3344, 5);
    } 
    char* tmpchararr_3343 = (char*)arrtmp_3344;
    memcpy(tmpchararr_3343, "APOL", 5);
     tmprc_2984 = tmpchararr_3343;
     tmpconstlift_1060 = tmprc_2984;
  } 
  {
    char* tmprc_2983 ;
    int* arrtmp_3342 = (int*)0;
    if (5 > 0) {
      arrtmp_3342 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3342, 5);
    } 
    char* tmpchararr_3341 = (char*)arrtmp_3342;
    memcpy(tmpchararr_3341, "APSG", 5);
     tmprc_2983 = tmpchararr_3341;
     tmpconstlift_1059 = tmprc_2983;
  } 
  {
    char* tmprc_2982 ;
    int* arrtmp_3340 = (int*)0;
    if (4 > 0) {
      arrtmp_3340 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3340, 4);
    } 
    char* tmpchararr_3339 = (char*)arrtmp_3340;
    memcpy(tmpchararr_3339, "APU", 4);
     tmprc_2982 = tmpchararr_3339;
     tmpconstlift_1058 = tmprc_2982;
  } 
  {
    char* tmprc_2981 ;
    int* arrtmp_3338 = (int*)0;
    if (5 > 0) {
      arrtmp_3338 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3338, 5);
    } 
    char* tmpchararr_3337 = (char*)arrtmp_3338;
    memcpy(tmpchararr_3337, "APWR", 5);
     tmprc_2981 = tmpchararr_3337;
     tmpconstlift_1057 = tmprc_2981;
  } 
  {
    char* tmprc_2980 ;
    int* arrtmp_3336 = (int*)0;
    if (5 > 0) {
      arrtmp_3336 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3336, 5);
    } 
    char* tmpchararr_3335 = (char*)arrtmp_3336;
    memcpy(tmpchararr_3335, "ARAY", 5);
     tmprc_2980 = tmpchararr_3335;
     tmpconstlift_1056 = tmprc_2980;
  } 
  {
    char* tmprc_2979 ;
    int* arrtmp_3334 = (int*)0;
    if (4 > 0) {
      arrtmp_3334 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3334, 4);
    } 
    char* tmpchararr_3333 = (char*)arrtmp_3334;
    memcpy(tmpchararr_3333, "ARB", 4);
     tmprc_2979 = tmpchararr_3333;
     tmpconstlift_1055 = tmprc_2979;
  } 
  {
    char* tmprc_2978 ;
    int* arrtmp_3332 = (int*)0;
    if (5 > 0) {
      arrtmp_3332 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3332, 5);
    } 
    char* tmpchararr_3331 = (char*)arrtmp_3332;
    memcpy(tmpchararr_3331, "ARBA", 5);
     tmprc_2978 = tmpchararr_3331;
     tmpconstlift_1054 = tmprc_2978;
  } 
  {
    char* tmprc_2977 ;
    int* arrtmp_3330 = (int*)0;
    if (5 > 0) {
      arrtmp_3330 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3330, 5);
    } 
    char* tmpchararr_3329 = (char*)arrtmp_3330;
    memcpy(tmpchararr_3329, "ARCC", 5);
     tmprc_2977 = tmpchararr_3329;
     tmpconstlift_1053 = tmprc_2977;
  } 
  {
    char* tmprc_2976 ;
    int* arrtmp_3328 = (int*)0;
    if (4 > 0) {
      arrtmp_3328 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3328, 4);
    } 
    char* tmpchararr_3327 = (char*)arrtmp_3328;
    memcpy(tmpchararr_3327, "ARD", 4);
     tmprc_2976 = tmpchararr_3327;
     tmpconstlift_1052 = tmprc_2976;
  } 
  {
    char* tmprc_2975 ;
    int* arrtmp_3326 = (int*)0;
    if (4 > 0) {
      arrtmp_3326 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3326, 4);
    } 
    char* tmpchararr_3325 = (char*)arrtmp_3326;
    memcpy(tmpchararr_3325, "ARE", 4);
     tmprc_2975 = tmpchararr_3325;
     tmpconstlift_1051 = tmprc_2975;
  } 
  {
    char* tmprc_2974 ;
    int* arrtmp_3324 = (int*)0;
    if (4 > 0) {
      arrtmp_3324 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3324, 4);
    } 
    char* tmpchararr_3323 = (char*)arrtmp_3324;
    memcpy(tmpchararr_3323, "ARG", 4);
     tmprc_2974 = tmpchararr_3323;
     tmpconstlift_1050 = tmprc_2974;
  } 
  {
    char* tmprc_2973 ;
    int* arrtmp_3322 = (int*)0;
    if (5 > 0) {
      arrtmp_3322 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3322, 5);
    } 
    char* tmpchararr_3321 = (char*)arrtmp_3322;
    memcpy(tmpchararr_3321, "ARGN", 5);
     tmprc_2973 = tmpchararr_3321;
     tmpconstlift_1049 = tmprc_2973;
  } 
  {
    char* tmprc_2972 ;
    int* arrtmp_3320 = (int*)0;
    if (4 > 0) {
      arrtmp_3320 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3320, 4);
    } 
    char* tmpchararr_3319 = (char*)arrtmp_3320;
    memcpy(tmpchararr_3319, "ARI", 4);
     tmprc_2972 = tmpchararr_3319;
     tmpconstlift_1048 = tmprc_2972;
  } 
  {
    char* tmprc_2971 ;
    int* arrtmp_3318 = (int*)0;
    if (5 > 0) {
      arrtmp_3318 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3318, 5);
    } 
    char* tmpchararr_3317 = (char*)arrtmp_3318;
    memcpy(tmpchararr_3317, "ARII", 5);
     tmprc_2971 = tmpchararr_3317;
     tmpconstlift_1047 = tmprc_2971;
  } 
  {
    char* tmprc_2970 ;
    int* arrtmp_3316 = (int*)0;
    if (4 > 0) {
      arrtmp_3316 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3316, 4);
    } 
    char* tmpchararr_3315 = (char*)arrtmp_3316;
    memcpy(tmpchararr_3315, "ARJ", 4);
     tmprc_2970 = tmpchararr_3315;
     tmpconstlift_1046 = tmprc_2970;
  } 
  {
    char* tmprc_2969 ;
    int* arrtmp_3314 = (int*)0;
    if (5 > 0) {
      arrtmp_3314 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3314, 5);
    } 
    char* tmpchararr_3313 = (char*)arrtmp_3314;
    memcpy(tmpchararr_3313, "ARLP", 5);
     tmprc_2969 = tmpchararr_3313;
     tmpconstlift_1045 = tmprc_2969;
  } 
  {
    char* tmprc_2968 ;
    int* arrtmp_3312 = (int*)0;
    if (4 > 0) {
      arrtmp_3312 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3312, 4);
    } 
    char* tmpchararr_3311 = (char*)arrtmp_3312;
    memcpy(tmpchararr_3311, "ARM", 4);
     tmprc_2968 = tmpchararr_3311;
     tmpconstlift_1044 = tmprc_2968;
  } 
  {
    char* tmprc_2967 ;
    int* arrtmp_3310 = (int*)0;
    if (5 > 0) {
      arrtmp_3310 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3310, 5);
    } 
    char* tmpchararr_3309 = (char*)arrtmp_3310;
    memcpy(tmpchararr_3309, "ARMH", 5);
     tmprc_2967 = tmpchararr_3309;
     tmpconstlift_1043 = tmprc_2967;
  } 
  {
    char* tmprc_2966 ;
    int* arrtmp_3308 = (int*)0;
    if (4 > 0) {
      arrtmp_3308 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3308, 4);
    } 
    char* tmpchararr_3307 = (char*)arrtmp_3308;
    memcpy(tmpchararr_3307, "ARO", 4);
     tmprc_2966 = tmpchararr_3307;
     tmpconstlift_1042 = tmprc_2966;
  } 
  {
    char* tmprc_2965 ;
    int* arrtmp_3306 = (int*)0;
    if (4 > 0) {
      arrtmp_3306 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3306, 4);
    } 
    char* tmpchararr_3305 = (char*)arrtmp_3306;
    memcpy(tmpchararr_3305, "ARP", 4);
     tmprc_2965 = tmpchararr_3305;
     tmpconstlift_1041 = tmprc_2965;
  } 
  {
    char* tmprc_2964 ;
    int* arrtmp_3304 = (int*)0;
    if (5 > 0) {
      arrtmp_3304 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3304, 5);
    } 
    char* tmpchararr_3303 = (char*)arrtmp_3304;
    memcpy(tmpchararr_3303, "ARRS", 5);
     tmprc_2964 = tmpchararr_3303;
     tmpconstlift_1040 = tmprc_2964;
  } 
  {
    char* tmprc_2963 ;
    int* arrtmp_3302 = (int*)0;
    if (5 > 0) {
      arrtmp_3302 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3302, 5);
    } 
    char* tmpchararr_3301 = (char*)arrtmp_3302;
    memcpy(tmpchararr_3301, "ARST", 5);
     tmprc_2963 = tmpchararr_3301;
     tmpconstlift_1039 = tmprc_2963;
  } 
  {
    char* tmprc_2962 ;
    int* arrtmp_3300 = (int*)0;
    if (4 > 0) {
      arrtmp_3300 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3300, 4);
    } 
    char* tmpchararr_3299 = (char*)arrtmp_3300;
    memcpy(tmpchararr_3299, "ART", 4);
     tmprc_2962 = tmpchararr_3299;
     tmpconstlift_1038 = tmprc_2962;
  } 
  {
    char* tmprc_2961 ;
    int* arrtmp_3298 = (int*)0;
    if (5 > 0) {
      arrtmp_3298 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3298, 5);
    } 
    char* tmpchararr_3297 = (char*)arrtmp_3298;
    memcpy(tmpchararr_3297, "ARUN", 5);
     tmprc_2961 = tmpchararr_3297;
     tmpconstlift_1037 = tmprc_2961;
  } 
  {
    char* tmprc_2960 ;
    int* arrtmp_3296 = (int*)0;
    if (4 > 0) {
      arrtmp_3296 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3296, 4);
    } 
    char* tmpchararr_3295 = (char*)arrtmp_3296;
    memcpy(tmpchararr_3295, "ARW", 4);
     tmprc_2960 = tmpchararr_3295;
     tmpconstlift_1036 = tmprc_2960;
  } 
  {
    char* tmprc_2959 ;
    int* arrtmp_3294 = (int*)0;
    if (4 > 0) {
      arrtmp_3294 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3294, 4);
    } 
    char* tmpchararr_3293 = (char*)arrtmp_3294;
    memcpy(tmpchararr_3293, "ASA", 4);
     tmprc_2959 = tmpchararr_3293;
     tmpconstlift_1035 = tmprc_2959;
  } 
  {
    char* tmprc_2958 ;
    int* arrtmp_3292 = (int*)0;
    if (5 > 0) {
      arrtmp_3292 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3292, 5);
    } 
    char* tmpchararr_3291 = (char*)arrtmp_3292;
    memcpy(tmpchararr_3291, "ASBC", 5);
     tmprc_2958 = tmpchararr_3291;
     tmpconstlift_1034 = tmprc_2958;
  } 
  {
    char* tmprc_2957 ;
    int* arrtmp_3290 = (int*)0;
    if (5 > 0) {
      arrtmp_3290 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3290, 5);
    } 
    char* tmpchararr_3289 = (char*)arrtmp_3290;
    memcpy(tmpchararr_3289, "ASCA", 5);
     tmprc_2957 = tmpchararr_3289;
     tmpconstlift_1033 = tmprc_2957;
  } 
  {
    char* tmprc_2956 ;
    int* arrtmp_3288 = (int*)0;
    if (5 > 0) {
      arrtmp_3288 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3288, 5);
    } 
    char* tmpchararr_3287 = (char*)arrtmp_3288;
    memcpy(tmpchararr_3287, "ASEI", 5);
     tmprc_2956 = tmpchararr_3287;
     tmpconstlift_1032 = tmprc_2956;
  } 
  {
    char* tmprc_2955 ;
    int* arrtmp_3286 = (int*)0;
    if (4 > 0) {
      arrtmp_3286 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3286, 4);
    } 
    char* tmpchararr_3285 = (char*)arrtmp_3286;
    memcpy(tmpchararr_3285, "ASF", 4);
     tmprc_2955 = tmpchararr_3285;
     tmpconstlift_1031 = tmprc_2955;
  } 
  {
    char* tmprc_2954 ;
    int* arrtmp_3284 = (int*)0;
    if (5 > 0) {
      arrtmp_3284 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3284, 5);
    } 
    char* tmpchararr_3283 = (char*)arrtmp_3284;
    memcpy(tmpchararr_3283, "ASFI", 5);
     tmprc_2954 = tmpchararr_3283;
     tmpconstlift_1030 = tmprc_2954;
  } 
  {
    char* tmprc_2953 ;
    int* arrtmp_3282 = (int*)0;
    if (4 > 0) {
      arrtmp_3282 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3282, 4);
    } 
    char* tmpchararr_3281 = (char*)arrtmp_3282;
    memcpy(tmpchararr_3281, "ASH", 4);
     tmprc_2953 = tmpchararr_3281;
     tmpconstlift_1029 = tmprc_2953;
  } 
  {
    char* tmprc_2952 ;
    int* arrtmp_3280 = (int*)0;
    if (5 > 0) {
      arrtmp_3280 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3280, 5);
    } 
    char* tmpchararr_3279 = (char*)arrtmp_3280;
    memcpy(tmpchararr_3279, "ASIA", 5);
     tmprc_2952 = tmpchararr_3279;
     tmpconstlift_1028 = tmprc_2952;
  } 
  {
    char* tmprc_2951 ;
    int* arrtmp_3278 = (int*)0;
    if (5 > 0) {
      arrtmp_3278 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3278, 5);
    } 
    char* tmpchararr_3277 = (char*)arrtmp_3278;
    memcpy(tmpchararr_3277, "ASMI", 5);
     tmprc_2951 = tmpchararr_3277;
     tmpconstlift_1027 = tmprc_2951;
  } 
  {
    char* tmprc_2950 ;
    int* arrtmp_3276 = (int*)0;
    if (5 > 0) {
      arrtmp_3276 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3276, 5);
    } 
    char* tmpchararr_3275 = (char*)arrtmp_3276;
    memcpy(tmpchararr_3275, "ASML", 5);
     tmprc_2950 = tmpchararr_3275;
     tmpconstlift_1026 = tmprc_2950;
  } 
  {
    char* tmprc_2949 ;
    int* arrtmp_3274 = (int*)0;
    if (5 > 0) {
      arrtmp_3274 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3274, 5);
    } 
    char* tmpchararr_3273 = (char*)arrtmp_3274;
    memcpy(tmpchararr_3273, "ASPS", 5);
     tmprc_2949 = tmpchararr_3273;
     tmpconstlift_1025 = tmprc_2949;
  } 
  {
    char* tmprc_2948 ;
    int* arrtmp_3272 = (int*)0;
    if (5 > 0) {
      arrtmp_3272 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3272, 5);
    } 
    char* tmpchararr_3271 = (char*)arrtmp_3272;
    memcpy(tmpchararr_3271, "ASTE", 5);
     tmprc_2948 = tmpchararr_3271;
     tmpconstlift_1024 = tmprc_2948;
  } 
  {
    char* tmprc_2947 ;
    int* arrtmp_3270 = (int*)0;
    if (5 > 0) {
      arrtmp_3270 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3270, 5);
    } 
    char* tmpchararr_3269 = (char*)arrtmp_3270;
    memcpy(tmpchararr_3269, "ATAC", 5);
     tmprc_2947 = tmpchararr_3269;
     tmpconstlift_1023 = tmprc_2947;
  } 
  {
    char* tmprc_2946 ;
    int* arrtmp_3268 = (int*)0;
    if (5 > 0) {
      arrtmp_3268 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3268, 5);
    } 
    char* tmpchararr_3267 = (char*)arrtmp_3268;
    memcpy(tmpchararr_3267, "ATHN", 5);
     tmprc_2946 = tmpchararr_3267;
     tmpconstlift_1022 = tmprc_2946;
  } 
  {
    char* tmprc_2945 ;
    int* arrtmp_3266 = (int*)0;
    if (5 > 0) {
      arrtmp_3266 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3266, 5);
    } 
    char* tmpchararr_3265 = (char*)arrtmp_3266;
    memcpy(tmpchararr_3265, "ATHR", 5);
     tmprc_2945 = tmpchararr_3265;
     tmpconstlift_1021 = tmprc_2945;
  } 
  {
    char* tmprc_2944 ;
    int* arrtmp_3264 = (int*)0;
    if (4 > 0) {
      arrtmp_3264 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3264, 4);
    } 
    char* tmpchararr_3263 = (char*)arrtmp_3264;
    memcpy(tmpchararr_3263, "ATI", 4);
     tmprc_2944 = tmpchararr_3263;
     tmpconstlift_1020 = tmprc_2944;
  } 
  {
    char* tmprc_2943 ;
    int* arrtmp_3262 = (int*)0;
    if (4 > 0) {
      arrtmp_3262 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3262, 4);
    } 
    char* tmpchararr_3261 = (char*)arrtmp_3262;
    memcpy(tmpchararr_3261, "ATK", 4);
     tmprc_2943 = tmpchararr_3261;
     tmpconstlift_1019 = tmprc_2943;
  } 
  {
    char* tmprc_2942 ;
    int* arrtmp_3260 = (int*)0;
    if (5 > 0) {
      arrtmp_3260 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3260, 5);
    } 
    char* tmpchararr_3259 = (char*)arrtmp_3260;
    memcpy(tmpchararr_3259, "ATLS", 5);
     tmprc_2942 = tmpchararr_3259;
     tmpconstlift_1018 = tmprc_2942;
  } 
  {
    char* tmprc_2941 ;
    int* arrtmp_3258 = (int*)0;
    if (5 > 0) {
      arrtmp_3258 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3258, 5);
    } 
    char* tmpchararr_3257 = (char*)arrtmp_3258;
    memcpy(tmpchararr_3257, "ATMI", 5);
     tmprc_2941 = tmpchararr_3257;
     tmpconstlift_1017 = tmprc_2941;
  } 
  {
    char* tmprc_2940 ;
    int* arrtmp_3256 = (int*)0;
    if (5 > 0) {
      arrtmp_3256 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3256, 5);
    } 
    char* tmpchararr_3255 = (char*)arrtmp_3256;
    memcpy(tmpchararr_3255, "ATNI", 5);
     tmprc_2940 = tmpchararr_3255;
     tmpconstlift_1016 = tmprc_2940;
  } 
  {
    char* tmprc_2939 ;
    int* arrtmp_3254 = (int*)0;
    if (4 > 0) {
      arrtmp_3254 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3254, 4);
    } 
    char* tmpchararr_3253 = (char*)arrtmp_3254;
    memcpy(tmpchararr_3253, "ATO", 4);
     tmprc_2939 = tmpchararr_3253;
     tmpconstlift_1015 = tmprc_2939;
  } 
  {
    char* tmprc_2938 ;
    int* arrtmp_3252 = (int*)0;
    if (5 > 0) {
      arrtmp_3252 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3252, 5);
    } 
    char* tmpchararr_3251 = (char*)arrtmp_3252;
    memcpy(tmpchararr_3251, "ATPG", 5);
     tmprc_2938 = tmpchararr_3251;
     tmpconstlift_1014 = tmprc_2938;
  } 
  {
    char* tmprc_2937 ;
    int* arrtmp_3250 = (int*)0;
    if (4 > 0) {
      arrtmp_3250 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3250, 4);
    } 
    char* tmpchararr_3249 = (char*)arrtmp_3250;
    memcpy(tmpchararr_3249, "ATR", 4);
     tmprc_2937 = tmpchararr_3249;
     tmpconstlift_1013 = tmprc_2937;
  } 
  {
    char* tmprc_2936 ;
    int* arrtmp_3248 = (int*)0;
    if (4 > 0) {
      arrtmp_3248 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3248, 4);
    } 
    char* tmpchararr_3247 = (char*)arrtmp_3248;
    memcpy(tmpchararr_3247, "ATU", 4);
     tmprc_2936 = tmpchararr_3247;
     tmpconstlift_1012 = tmprc_2936;
  } 
  {
    char* tmprc_2935 ;
    int* arrtmp_3246 = (int*)0;
    if (5 > 0) {
      arrtmp_3246 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3246, 5);
    } 
    char* tmpchararr_3245 = (char*)arrtmp_3246;
    memcpy(tmpchararr_3245, "ATVI", 5);
     tmprc_2935 = tmpchararr_3245;
     tmpconstlift_1011 = tmprc_2935;
  } 
  {
    char* tmprc_2934 ;
    int* arrtmp_3244 = (int*)0;
    if (4 > 0) {
      arrtmp_3244 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3244, 4);
    } 
    char* tmpchararr_3243 = (char*)arrtmp_3244;
    memcpy(tmpchararr_3243, "ATW", 4);
     tmprc_2934 = tmpchararr_3243;
     tmpconstlift_1010 = tmprc_2934;
  } 
  {
    char* tmprc_2933 ;
    int* arrtmp_3242 = (int*)0;
    if (3 > 0) {
      arrtmp_3242 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 3) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3242, 3);
    } 
    char* tmpchararr_3241 = (char*)arrtmp_3242;
    memcpy(tmpchararr_3241, "AU", 3);
     tmprc_2933 = tmpchararr_3241;
     tmpconstlift_1009 = tmprc_2933;
  } 
  {
    char* tmprc_2932 ;
    int* arrtmp_3240 = (int*)0;
    if (4 > 0) {
      arrtmp_3240 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3240, 4);
    } 
    char* tmpchararr_3239 = (char*)arrtmp_3240;
    memcpy(tmpchararr_3239, "AUO", 4);
     tmprc_2932 = tmpchararr_3239;
     tmpconstlift_1008 = tmprc_2932;
  } 
  {
    char* tmprc_2931 ;
    int* arrtmp_3238 = (int*)0;
    if (5 > 0) {
      arrtmp_3238 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3238, 5);
    } 
    char* tmpchararr_3237 = (char*)arrtmp_3238;
    memcpy(tmpchararr_3237, "AUXL", 5);
     tmprc_2931 = tmpchararr_3237;
     tmpconstlift_1007 = tmprc_2931;
  } 
  {
    char* tmprc_2930 ;
    int* arrtmp_3236 = (int*)0;
    if (4 > 0) {
      arrtmp_3236 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3236, 4);
    } 
    char* tmpchararr_3235 = (char*)arrtmp_3236;
    memcpy(tmpchararr_3235, "AUY", 4);
     tmprc_2930 = tmpchararr_3235;
     tmpconstlift_1006 = tmprc_2930;
  } 
  {
    char* tmprc_2929 ;
    int* arrtmp_3234 = (int*)0;
    if (4 > 0) {
      arrtmp_3234 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3234, 4);
    } 
    char* tmpchararr_3233 = (char*)arrtmp_3234;
    memcpy(tmpchararr_3233, "AVA", 4);
     tmprc_2929 = tmpchararr_3233;
     tmpconstlift_1005 = tmprc_2929;
  } 
  {
    char* tmprc_2928 ;
    int* arrtmp_3232 = (int*)0;
    if (5 > 0) {
      arrtmp_3232 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3232, 5);
    } 
    char* tmpchararr_3231 = (char*)arrtmp_3232;
    memcpy(tmpchararr_3231, "AVAV", 5);
     tmprc_2928 = tmpchararr_3231;
     tmpconstlift_1004 = tmprc_2928;
  } 
  {
    char* tmprc_2927 ;
    int* arrtmp_3230 = (int*)0;
    if (4 > 0) {
      arrtmp_3230 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3230, 4);
    } 
    char* tmpchararr_3229 = (char*)arrtmp_3230;
    memcpy(tmpchararr_3229, "AVB", 4);
     tmprc_2927 = tmpchararr_3229;
     tmpconstlift_1003 = tmprc_2927;
  } 
  {
    char* tmprc_2926 ;
    int* arrtmp_3228 = (int*)0;
    if (4 > 0) {
      arrtmp_3228 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3228, 4);
    } 
    char* tmpchararr_3227 = (char*)arrtmp_3228;
    memcpy(tmpchararr_3227, "AVD", 4);
     tmprc_2926 = tmpchararr_3227;
     tmpconstlift_1002 = tmprc_2926;
  } 
  {
    char* tmprc_2925 ;
    int* arrtmp_3226 = (int*)0;
    if (5 > 0) {
      arrtmp_3226 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3226, 5);
    } 
    char* tmpchararr_3225 = (char*)arrtmp_3226;
    memcpy(tmpchararr_3225, "AVGO", 5);
     tmprc_2925 = tmpchararr_3225;
     tmpconstlift_1001 = tmprc_2925;
  } 
  {
    char* tmprc_2924 ;
    int* arrtmp_3224 = (int*)0;
    if (5 > 0) {
      arrtmp_3224 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3224, 5);
    } 
    char* tmpchararr_3223 = (char*)arrtmp_3224;
    memcpy(tmpchararr_3223, "AVID", 5);
     tmprc_2924 = tmpchararr_3223;
     tmpconstlift_1000 = tmprc_2924;
  } 
  {
    char* tmprc_2923 ;
    int* arrtmp_3222 = (int*)0;
    if (4 > 0) {
      arrtmp_3222 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3222, 4);
    } 
    char* tmpchararr_3221 = (char*)arrtmp_3222;
    memcpy(tmpchararr_3221, "AVP", 4);
     tmprc_2923 = tmpchararr_3221;
     tmpconstlift_999 = tmprc_2923;
  } 
  {
    char* tmprc_2922 ;
    int* arrtmp_3220 = (int*)0;
    if (4 > 0) {
      arrtmp_3220 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3220, 4);
    } 
    char* tmpchararr_3219 = (char*)arrtmp_3220;
    memcpy(tmpchararr_3219, "AVT", 4);
     tmprc_2922 = tmpchararr_3219;
     tmpconstlift_998 = tmprc_2922;
  } 
  {
    char* tmprc_2921 ;
    int* arrtmp_3218 = (int*)0;
    if (5 > 0) {
      arrtmp_3218 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3218, 5);
    } 
    char* tmpchararr_3217 = (char*)arrtmp_3218;
    memcpy(tmpchararr_3217, "AVTR", 5);
     tmprc_2921 = tmpchararr_3217;
     tmpconstlift_997 = tmprc_2921;
  } 
  {
    char* tmprc_2920 ;
    int* arrtmp_3216 = (int*)0;
    if (4 > 0) {
      arrtmp_3216 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3216, 4);
    } 
    char* tmpchararr_3215 = (char*)arrtmp_3216;
    memcpy(tmpchararr_3215, "AVY", 4);
     tmprc_2920 = tmpchararr_3215;
     tmpconstlift_996 = tmprc_2920;
  } 
  {
    char* tmprc_2919 ;
    int* arrtmp_3214 = (int*)0;
    if (4 > 0) {
      arrtmp_3214 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3214, 4);
    } 
    char* tmpchararr_3213 = (char*)arrtmp_3214;
    memcpy(tmpchararr_3213, "AWC", 4);
     tmprc_2919 = tmpchararr_3213;
     tmpconstlift_995 = tmprc_2919;
  } 
  {
    char* tmprc_2918 ;
    int* arrtmp_3212 = (int*)0;
    if (4 > 0) {
      arrtmp_3212 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3212, 4);
    } 
    char* tmpchararr_3211 = (char*)arrtmp_3212;
    memcpy(tmpchararr_3211, "AWH", 4);
     tmprc_2918 = tmpchararr_3211;
     tmpconstlift_994 = tmprc_2918;
  } 
  {
    char* tmprc_2917 ;
    int* arrtmp_3210 = (int*)0;
    if (4 > 0) {
      arrtmp_3210 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3210, 4);
    } 
    char* tmpchararr_3209 = (char*)arrtmp_3210;
    memcpy(tmpchararr_3209, "AWI", 4);
     tmprc_2917 = tmpchararr_3209;
     tmpconstlift_993 = tmprc_2917;
  } 
  {
    char* tmprc_2916 ;
    int* arrtmp_3208 = (int*)0;
    if (4 > 0) {
      arrtmp_3208 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3208, 4);
    } 
    char* tmpchararr_3207 = (char*)arrtmp_3208;
    memcpy(tmpchararr_3207, "AWK", 4);
     tmprc_2916 = tmpchararr_3207;
     tmpconstlift_992 = tmprc_2916;
  } 
  {
    char* tmprc_2915 ;
    int* arrtmp_3206 = (int*)0;
    if (6 > 0) {
      arrtmp_3206 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 6) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3206, 6);
    } 
    char* tmpchararr_3205 = (char*)arrtmp_3206;
    memcpy(tmpchararr_3205, "AXAHY", 6);
     tmprc_2915 = tmpchararr_3205;
     tmpconstlift_991 = tmprc_2915;
  } 
  {
    char* tmprc_2914 ;
    int* arrtmp_3204 = (int*)0;
    if (4 > 0) {
      arrtmp_3204 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3204, 4);
    } 
    char* tmpchararr_3203 = (char*)arrtmp_3204;
    memcpy(tmpchararr_3203, "AXE", 4);
     tmprc_2914 = tmpchararr_3203;
     tmpconstlift_990 = tmprc_2914;
  } 
  {
    char* tmprc_2913 ;
    int* arrtmp_3202 = (int*)0;
    if (4 > 0) {
      arrtmp_3202 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3202, 4);
    } 
    char* tmpchararr_3201 = (char*)arrtmp_3202;
    memcpy(tmpchararr_3201, "AXL", 4);
     tmprc_2913 = tmpchararr_3201;
     tmpconstlift_989 = tmprc_2913;
  } 
  {
    char* tmprc_2912 ;
    int* arrtmp_3200 = (int*)0;
    if (4 > 0) {
      arrtmp_3200 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3200, 4);
    } 
    char* tmpchararr_3199 = (char*)arrtmp_3200;
    memcpy(tmpchararr_3199, "AXP", 4);
     tmprc_2912 = tmpchararr_3199;
     tmpconstlift_988 = tmprc_2912;
  } 
  {
    char* tmprc_2911 ;
    int* arrtmp_3198 = (int*)0;
    if (4 > 0) {
      arrtmp_3198 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3198, 4);
    } 
    char* tmpchararr_3197 = (char*)arrtmp_3198;
    memcpy(tmpchararr_3197, "AXS", 4);
     tmprc_2911 = tmpchararr_3197;
     tmpconstlift_987 = tmprc_2911;
  } 
  {
    char* tmprc_2910 ;
    int* arrtmp_3196 = (int*)0;
    if (4 > 0) {
      arrtmp_3196 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3196, 4);
    } 
    char* tmpchararr_3195 = (char*)arrtmp_3196;
    memcpy(tmpchararr_3195, "AYE", 4);
     tmprc_2910 = tmpchararr_3195;
     tmpconstlift_986 = tmprc_2910;
  } 
  {
    char* tmprc_2909 ;
    int* arrtmp_3194 = (int*)0;
    if (4 > 0) {
      arrtmp_3194 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3194, 4);
    } 
    char* tmpchararr_3193 = (char*)arrtmp_3194;
    memcpy(tmpchararr_3193, "AYI", 4);
     tmprc_2909 = tmpchararr_3193;
     tmpconstlift_985 = tmprc_2909;
  } 
  {
    char* tmprc_2908 ;
    int* arrtmp_3192 = (int*)0;
    if (4 > 0) {
      arrtmp_3192 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3192, 4);
    } 
    char* tmpchararr_3191 = (char*)arrtmp_3192;
    memcpy(tmpchararr_3191, "AYR", 4);
     tmprc_2908 = tmpchararr_3191;
     tmpconstlift_984 = tmprc_2908;
  } 
  {
    char* tmprc_2907 ;
    int* arrtmp_3190 = (int*)0;
    if (4 > 0) {
      arrtmp_3190 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3190, 4);
    } 
    char* tmpchararr_3189 = (char*)arrtmp_3190;
    memcpy(tmpchararr_3189, "AZN", 4);
     tmprc_2907 = tmpchararr_3189;
     tmpconstlift_983 = tmprc_2907;
  } 
  {
    char* tmprc_2906 ;
    int* arrtmp_3188 = (int*)0;
    if (4 > 0) {
      arrtmp_3188 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 4) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3188, 4);
    } 
    char* tmpchararr_3187 = (char*)arrtmp_3188;
    memcpy(tmpchararr_3187, "AZO", 4);
     tmprc_2906 = tmpchararr_3187;
     tmpconstlift_982 = tmprc_2906;
  } 
  {
    char* tmprc_2905 ;
    int* arrtmp_3186 = (int*)0;
    if (6 > 0) {
      arrtmp_3186 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 6) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3186, 6);
    } 
    char* tmpchararr_3185 = (char*)arrtmp_3186;
    memcpy(tmpchararr_3185, "AZSEY", 6);
     tmprc_2905 = tmpchararr_3185;
     tmpconstlift_981 = tmprc_2905;
  } 
  {
    char* tmprc_2904 ;
    int* arrtmp_3184 = (int*)0;
    if (10 > 0) {
      arrtmp_3184 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 10) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3184, 10);
    } 
    char* tmpchararr_3183 = (char*)arrtmp_3184;
    memcpy(tmpchararr_3183, "NETSTRM: ", 10);
     tmprc_2904 = tmpchararr_3183;
     tmpconstlift_980 = tmprc_2904;
  } 
  {
    char* tmprc_2903 ;
    int* arrtmp_3182 = (int*)0;
    if (2 > 0) {
      arrtmp_3182 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3182, 2);
    } 
    char* tmpchararr_3181 = (char*)arrtmp_3182;
    memcpy(tmpchararr_3181, "(", 2);
     tmprc_2903 = tmpchararr_3181;
     tmpconstlift_979 = tmprc_2903;
  } 
  {
    char* tmprc_2902 ;
    int* arrtmp_3180 = (int*)0;
    if (7 > 0) {
      arrtmp_3180 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 7) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3180, 7);
    } 
    char* tmpchararr_3179 = (char*)arrtmp_3180;
    memcpy(tmpchararr_3179, "PRICE=", 7);
     tmprc_2902 = tmpchararr_3179;
     tmpconstlift_978 = tmprc_2902;
  } 
  {
    char* tmprc_2901 ;
    int* arrtmp_3178 = (int*)0;
    if (3 > 0) {
      arrtmp_3178 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 3) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3178, 3);
    } 
    char* tmpchararr_3177 = (char*)arrtmp_3178;
    memcpy(tmpchararr_3177, ", ", 3);
     tmprc_2901 = tmpchararr_3177;
     tmpconstlift_977 = tmprc_2901;
  } 
  {
    char* tmprc_2900 ;
    int* arrtmp_3176 = (int*)0;
    if (5 > 0) {
      arrtmp_3176 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 5) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3176, 5);
    } 
    char* tmpchararr_3175 = (char*)arrtmp_3176;
    memcpy(tmpchararr_3175, "SYM=", 5);
     tmprc_2900 = tmpchararr_3175;
     tmpconstlift_976 = tmprc_2900;
  } 
  {
    char* tmprc_2899 ;
    int* arrtmp_3174 = (int*)0;
    if (3 > 0) {
      arrtmp_3174 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 3) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3174, 3);
    } 
    char* tmpchararr_3173 = (char*)arrtmp_3174;
    memcpy(tmpchararr_3173, ", ", 3);
     tmprc_2899 = tmpchararr_3173;
     tmpconstlift_975 = tmprc_2899;
  } 
  {
    char* tmprc_2898 ;
    int* arrtmp_3172 = (int*)0;
    if (6 > 0) {
      arrtmp_3172 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 6) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3172, 6);
    } 
    char* tmpchararr_3171 = (char*)arrtmp_3172;
    memcpy(tmpchararr_3171, "TIME=", 6);
     tmprc_2898 = tmpchararr_3171;
     tmpconstlift_974 = tmprc_2898;
  } 
  {
    char* tmprc_2897 ;
    int* arrtmp_3170 = (int*)0;
    if (3 > 0) {
      arrtmp_3170 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 3) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3170, 3);
    } 
    char* tmpchararr_3169 = (char*)arrtmp_3170;
    memcpy(tmpchararr_3169, ", ", 3);
     tmprc_2897 = tmpchararr_3169;
     tmpconstlift_973 = tmprc_2897;
  } 
  {
    char* tmprc_2896 ;
    int* arrtmp_3168 = (int*)0;
    if (8 > 0) {
      arrtmp_3168 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 8) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3168, 8);
    } 
    char* tmpchararr_3167 = (char*)arrtmp_3168;
    memcpy(tmpchararr_3167, "VOLUME=", 8);
     tmprc_2896 = tmpchararr_3167;
     tmpconstlift_972 = tmprc_2896;
  } 
  {
    char* tmprc_2895 ;
    int* arrtmp_3166 = (int*)0;
    if (2 > 0) {
      arrtmp_3166 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3166, 2);
    } 
    char* tmpchararr_3165 = (char*)arrtmp_3166;
    memcpy(tmpchararr_3165, ")", 2);
     tmprc_2895 = tmpchararr_3165;
     tmpconstlift_971 = tmprc_2895;
  } 
  {
    char* tmprc_2894 ;
    int* arrtmp_3164 = (int*)0;
    if (2 > 0) {
      arrtmp_3164 = (int*)((char*)WSMALLOC_SCALAR((sizeof(char) * 2) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3164, 2);
    } 
    char* tmpchararr_3163 = (char*)arrtmp_3164;
    memcpy(tmpchararr_3163, "\n", 2);
     tmprc_2894 = tmpchararr_3163;
     tmpconstlift_970 = tmprc_2894;
  } 
   Unix_fflush_15 = 0;
  {
    char tmpsmp_1857 ;
     tmpsmp_1857 = 0;
     stdout_14 = ws_get_stdout() /* foreign app */ ;
  } 
  {
    struct tuptyp_2067** tmprc_2075 ;
    {
      struct tuptyp_2067** tmp_2081 ;
      int* arrtmp_3730 = (int*)0;
      if (1 > 0) {
        arrtmp_3730 = (int*)((char*)WSMALLOC((sizeof(struct tuptyp_2067*) * 1) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
        SETARRLEN(arrtmp_3730, 1);
        struct tuptyp_2067** arrtmpb_3732 = (struct tuptyp_2067**)arrtmp_3730;
        int lenmin1_3733 = 1 - 1;
        {
           int i_3731;
          for (i_3731 = 0; i_3731 <= lenmin1_3733; i_3731++) {
            arrtmpb_3732[i_3731] = ((struct tuptyp_2067*)0);
          } 
        } 
      } 
       tmp_2081 = (struct tuptyp_2067**)arrtmp_3730;
      {
        int end_2080 ;
         end_2080 = (1 - 1);
        {
           int ind_2083;
          for (ind_2083 = 0; ind_2083 <= end_2080; ind_2083++) {
            {
              struct tuptyp_2067* tmp_2082 ;
               tmp_2082 = tmp_2081[ind_2083];
            } 
          } 
        } 
         tmprc_2075 = tmp_2081;
      } 
    } 
     buf2_20 = tmprc_2075;
  } 
  {
    struct tuptyp_2067** tmprc_2074 ;
    {
      struct tuptyp_2067** tmp_2077 ;
      int* arrtmp_3726 = (int*)0;
      if (1 > 0) {
        arrtmp_3726 = (int*)((char*)WSMALLOC((sizeof(struct tuptyp_2067*) * 1) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
        SETARRLEN(arrtmp_3726, 1);
        struct tuptyp_2067** arrtmpb_3728 = (struct tuptyp_2067**)arrtmp_3726;
        int lenmin1_3729 = 1 - 1;
        {
           int i_3727;
          for (i_3727 = 0; i_3727 <= lenmin1_3729; i_3727++) {
            arrtmpb_3728[i_3727] = ((struct tuptyp_2067*)0);
          } 
        } 
      } 
       tmp_2077 = (struct tuptyp_2067**)arrtmp_3726;
      {
        int end_2076 ;
         end_2076 = (1 - 1);
        {
           int ind_2079;
          for (ind_2079 = 0; ind_2079 <= end_2076; ind_2079++) {
            {
              struct tuptyp_2067* tmp_2078 ;
               tmp_2078 = tmp_2077[ind_2079];
            } 
          } 
        } 
         tmprc_2074 = tmp_2077;
      } 
    } 
     buf1_19 = tmprc_2074;
  } 
  {
    float* tmprc_2275 ;
    int* arrtmp_3717 = (int*)0;
    if (248 > 0) {
      arrtmp_3717 = (int*)((char*)WSMALLOC_SCALAR((sizeof(float) * 248) + RCSIZE + ARRLENSIZE) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3717, 248);
      float* arrtmpb_3719 = (float*)arrtmp_3717;
      int lenmin1_3720 = 248 - 1;
      {
         int i_3718;
        for (i_3718 = 0; i_3718 <= lenmin1_3720; i_3718++) {
          arrtmpb_3719[i_3718] = 50.0F;
        } 
      } 
    } 
     tmprc_2275 = (float*)arrtmp_3717;
     lastprice_83 = tmprc_2275;
  } 
  {
    char** tmparr_137 ;
    {
      char** tmprc_2277 ;
      int* arrtmp_3716 = (int*)0;
      if (248 > 0) {
        arrtmp_3716 = (int*)((char*)WSCALLOC((sizeof(char*) * 248) + RCSIZE + ARRLENSIZE, 1) + RCSIZE + ARRLENSIZE);
        SETARRLEN(arrtmp_3716, 248);
      } 
       tmprc_2277 = (char**)arrtmp_3716;
       tmparr_137 = tmprc_2277;
    } 
    {
      char* tmp_2773 ;
       tmp_2773 = tmparr_137[0];
      {
        char* tmp_2774 ;
         tmp_2774 = tmpconstlift_1228;
        tmparr_137[0] = tmp_2774;
      } 
    } 
    {
      char* tmp_2771 ;
       tmp_2771 = tmparr_137[1];
      {
        char* tmp_2772 ;
         tmp_2772 = tmpconstlift_1227;
        tmparr_137[1] = tmp_2772;
      } 
    } 
    {
      char* tmp_2769 ;
       tmp_2769 = tmparr_137[2];
      {
        char* tmp_2770 ;
         tmp_2770 = tmpconstlift_1226;
        tmparr_137[2] = tmp_2770;
      } 
    } 
    {
      char* tmp_2767 ;
       tmp_2767 = tmparr_137[3];
      {
        char* tmp_2768 ;
         tmp_2768 = tmpconstlift_1225;
        tmparr_137[3] = tmp_2768;
      } 
    } 
    {
      char* tmp_2765 ;
       tmp_2765 = tmparr_137[4];
      {
        char* tmp_2766 ;
         tmp_2766 = tmpconstlift_1224;
        tmparr_137[4] = tmp_2766;
      } 
    } 
    {
      char* tmp_2763 ;
       tmp_2763 = tmparr_137[5];
      {
        char* tmp_2764 ;
         tmp_2764 = tmpconstlift_1223;
        tmparr_137[5] = tmp_2764;
      } 
    } 
    {
      char* tmp_2761 ;
       tmp_2761 = tmparr_137[6];
      {
        char* tmp_2762 ;
         tmp_2762 = tmpconstlift_1222;
        tmparr_137[6] = tmp_2762;
      } 
    } 
    {
      char* tmp_2759 ;
       tmp_2759 = tmparr_137[7];
      {
        char* tmp_2760 ;
         tmp_2760 = tmpconstlift_1221;
        tmparr_137[7] = tmp_2760;
      } 
    } 
    {
      char* tmp_2757 ;
       tmp_2757 = tmparr_137[8];
      {
        char* tmp_2758 ;
         tmp_2758 = tmpconstlift_1220;
        tmparr_137[8] = tmp_2758;
      } 
    } 
    {
      char* tmp_2755 ;
       tmp_2755 = tmparr_137[9];
      {
        char* tmp_2756 ;
         tmp_2756 = tmpconstlift_1219;
        tmparr_137[9] = tmp_2756;
      } 
    } 
    {
      char* tmp_2753 ;
       tmp_2753 = tmparr_137[10];
      {
        char* tmp_2754 ;
         tmp_2754 = tmpconstlift_1218;
        tmparr_137[10] = tmp_2754;
      } 
    } 
    {
      char* tmp_2751 ;
       tmp_2751 = tmparr_137[11];
      {
        char* tmp_2752 ;
         tmp_2752 = tmpconstlift_1217;
        tmparr_137[11] = tmp_2752;
      } 
    } 
    {
      char* tmp_2749 ;
       tmp_2749 = tmparr_137[12];
      {
        char* tmp_2750 ;
         tmp_2750 = tmpconstlift_1216;
        tmparr_137[12] = tmp_2750;
      } 
    } 
    {
      char* tmp_2747 ;
       tmp_2747 = tmparr_137[13];
      {
        char* tmp_2748 ;
         tmp_2748 = tmpconstlift_1215;
        tmparr_137[13] = tmp_2748;
      } 
    } 
    {
      char* tmp_2745 ;
       tmp_2745 = tmparr_137[14];
      {
        char* tmp_2746 ;
         tmp_2746 = tmpconstlift_1214;
        tmparr_137[14] = tmp_2746;
      } 
    } 
    {
      char* tmp_2743 ;
       tmp_2743 = tmparr_137[15];
      {
        char* tmp_2744 ;
         tmp_2744 = tmpconstlift_1213;
        tmparr_137[15] = tmp_2744;
      } 
    } 
    {
      char* tmp_2741 ;
       tmp_2741 = tmparr_137[16];
      {
        char* tmp_2742 ;
         tmp_2742 = tmpconstlift_1212;
        tmparr_137[16] = tmp_2742;
      } 
    } 
    {
      char* tmp_2739 ;
       tmp_2739 = tmparr_137[17];
      {
        char* tmp_2740 ;
         tmp_2740 = tmpconstlift_1211;
        tmparr_137[17] = tmp_2740;
      } 
    } 
    {
      char* tmp_2737 ;
       tmp_2737 = tmparr_137[18];
      {
        char* tmp_2738 ;
         tmp_2738 = tmpconstlift_1210;
        tmparr_137[18] = tmp_2738;
      } 
    } 
    {
      char* tmp_2735 ;
       tmp_2735 = tmparr_137[19];
      {
        char* tmp_2736 ;
         tmp_2736 = tmpconstlift_1209;
        tmparr_137[19] = tmp_2736;
      } 
    } 
    {
      char* tmp_2733 ;
       tmp_2733 = tmparr_137[20];
      {
        char* tmp_2734 ;
         tmp_2734 = tmpconstlift_1208;
        tmparr_137[20] = tmp_2734;
      } 
    } 
    {
      char* tmp_2731 ;
       tmp_2731 = tmparr_137[21];
      {
        char* tmp_2732 ;
         tmp_2732 = tmpconstlift_1207;
        tmparr_137[21] = tmp_2732;
      } 
    } 
    {
      char* tmp_2729 ;
       tmp_2729 = tmparr_137[22];
      {
        char* tmp_2730 ;
         tmp_2730 = tmpconstlift_1206;
        tmparr_137[22] = tmp_2730;
      } 
    } 
    {
      char* tmp_2727 ;
       tmp_2727 = tmparr_137[23];
      {
        char* tmp_2728 ;
         tmp_2728 = tmpconstlift_1205;
        tmparr_137[23] = tmp_2728;
      } 
    } 
    {
      char* tmp_2725 ;
       tmp_2725 = tmparr_137[24];
      {
        char* tmp_2726 ;
         tmp_2726 = tmpconstlift_1204;
        tmparr_137[24] = tmp_2726;
      } 
    } 
    {
      char* tmp_2723 ;
       tmp_2723 = tmparr_137[25];
      {
        char* tmp_2724 ;
         tmp_2724 = tmpconstlift_1203;
        tmparr_137[25] = tmp_2724;
      } 
    } 
    {
      char* tmp_2721 ;
       tmp_2721 = tmparr_137[26];
      {
        char* tmp_2722 ;
         tmp_2722 = tmpconstlift_1202;
        tmparr_137[26] = tmp_2722;
      } 
    } 
    {
      char* tmp_2719 ;
       tmp_2719 = tmparr_137[27];
      {
        char* tmp_2720 ;
         tmp_2720 = tmpconstlift_1201;
        tmparr_137[27] = tmp_2720;
      } 
    } 
    {
      char* tmp_2717 ;
       tmp_2717 = tmparr_137[28];
      {
        char* tmp_2718 ;
         tmp_2718 = tmpconstlift_1200;
        tmparr_137[28] = tmp_2718;
      } 
    } 
    {
      char* tmp_2715 ;
       tmp_2715 = tmparr_137[29];
      {
        char* tmp_2716 ;
         tmp_2716 = tmpconstlift_1199;
        tmparr_137[29] = tmp_2716;
      } 
    } 
    {
      char* tmp_2713 ;
       tmp_2713 = tmparr_137[30];
      {
        char* tmp_2714 ;
         tmp_2714 = tmpconstlift_1198;
        tmparr_137[30] = tmp_2714;
      } 
    } 
    {
      char* tmp_2711 ;
       tmp_2711 = tmparr_137[31];
      {
        char* tmp_2712 ;
         tmp_2712 = tmpconstlift_1197;
        tmparr_137[31] = tmp_2712;
      } 
    } 
    {
      char* tmp_2709 ;
       tmp_2709 = tmparr_137[32];
      {
        char* tmp_2710 ;
         tmp_2710 = tmpconstlift_1196;
        tmparr_137[32] = tmp_2710;
      } 
    } 
    {
      char* tmp_2707 ;
       tmp_2707 = tmparr_137[33];
      {
        char* tmp_2708 ;
         tmp_2708 = tmpconstlift_1195;
        tmparr_137[33] = tmp_2708;
      } 
    } 
    {
      char* tmp_2705 ;
       tmp_2705 = tmparr_137[34];
      {
        char* tmp_2706 ;
         tmp_2706 = tmpconstlift_1194;
        tmparr_137[34] = tmp_2706;
      } 
    } 
    {
      char* tmp_2703 ;
       tmp_2703 = tmparr_137[35];
      {
        char* tmp_2704 ;
         tmp_2704 = tmpconstlift_1193;
        tmparr_137[35] = tmp_2704;
      } 
    } 
    {
      char* tmp_2701 ;
       tmp_2701 = tmparr_137[36];
      {
        char* tmp_2702 ;
         tmp_2702 = tmpconstlift_1192;
        tmparr_137[36] = tmp_2702;
      } 
    } 
    {
      char* tmp_2699 ;
       tmp_2699 = tmparr_137[37];
      {
        char* tmp_2700 ;
         tmp_2700 = tmpconstlift_1191;
        tmparr_137[37] = tmp_2700;
      } 
    } 
    {
      char* tmp_2697 ;
       tmp_2697 = tmparr_137[38];
      {
        char* tmp_2698 ;
         tmp_2698 = tmpconstlift_1190;
        tmparr_137[38] = tmp_2698;
      } 
    } 
    {
      char* tmp_2695 ;
       tmp_2695 = tmparr_137[39];
      {
        char* tmp_2696 ;
         tmp_2696 = tmpconstlift_1189;
        tmparr_137[39] = tmp_2696;
      } 
    } 
    {
      char* tmp_2693 ;
       tmp_2693 = tmparr_137[40];
      {
        char* tmp_2694 ;
         tmp_2694 = tmpconstlift_1188;
        tmparr_137[40] = tmp_2694;
      } 
    } 
    {
      char* tmp_2691 ;
       tmp_2691 = tmparr_137[41];
      {
        char* tmp_2692 ;
         tmp_2692 = tmpconstlift_1187;
        tmparr_137[41] = tmp_2692;
      } 
    } 
    {
      char* tmp_2689 ;
       tmp_2689 = tmparr_137[42];
      {
        char* tmp_2690 ;
         tmp_2690 = tmpconstlift_1186;
        tmparr_137[42] = tmp_2690;
      } 
    } 
    {
      char* tmp_2687 ;
       tmp_2687 = tmparr_137[43];
      {
        char* tmp_2688 ;
         tmp_2688 = tmpconstlift_1185;
        tmparr_137[43] = tmp_2688;
      } 
    } 
    {
      char* tmp_2685 ;
       tmp_2685 = tmparr_137[44];
      {
        char* tmp_2686 ;
         tmp_2686 = tmpconstlift_1184;
        tmparr_137[44] = tmp_2686;
      } 
    } 
    {
      char* tmp_2683 ;
       tmp_2683 = tmparr_137[45];
      {
        char* tmp_2684 ;
         tmp_2684 = tmpconstlift_1183;
        tmparr_137[45] = tmp_2684;
      } 
    } 
    {
      char* tmp_2681 ;
       tmp_2681 = tmparr_137[46];
      {
        char* tmp_2682 ;
         tmp_2682 = tmpconstlift_1182;
        tmparr_137[46] = tmp_2682;
      } 
    } 
    {
      char* tmp_2679 ;
       tmp_2679 = tmparr_137[47];
      {
        char* tmp_2680 ;
         tmp_2680 = tmpconstlift_1181;
        tmparr_137[47] = tmp_2680;
      } 
    } 
    {
      char* tmp_2677 ;
       tmp_2677 = tmparr_137[48];
      {
        char* tmp_2678 ;
         tmp_2678 = tmpconstlift_1180;
        tmparr_137[48] = tmp_2678;
      } 
    } 
    {
      char* tmp_2675 ;
       tmp_2675 = tmparr_137[49];
      {
        char* tmp_2676 ;
         tmp_2676 = tmpconstlift_1179;
        tmparr_137[49] = tmp_2676;
      } 
    } 
    {
      char* tmp_2673 ;
       tmp_2673 = tmparr_137[50];
      {
        char* tmp_2674 ;
         tmp_2674 = tmpconstlift_1178;
        tmparr_137[50] = tmp_2674;
      } 
    } 
    {
      char* tmp_2671 ;
       tmp_2671 = tmparr_137[51];
      {
        char* tmp_2672 ;
         tmp_2672 = tmpconstlift_1177;
        tmparr_137[51] = tmp_2672;
      } 
    } 
    {
      char* tmp_2669 ;
       tmp_2669 = tmparr_137[52];
      {
        char* tmp_2670 ;
         tmp_2670 = tmpconstlift_1176;
        tmparr_137[52] = tmp_2670;
      } 
    } 
    {
      char* tmp_2667 ;
       tmp_2667 = tmparr_137[53];
      {
        char* tmp_2668 ;
         tmp_2668 = tmpconstlift_1175;
        tmparr_137[53] = tmp_2668;
      } 
    } 
    {
      char* tmp_2665 ;
       tmp_2665 = tmparr_137[54];
      {
        char* tmp_2666 ;
         tmp_2666 = tmpconstlift_1174;
        tmparr_137[54] = tmp_2666;
      } 
    } 
    {
      char* tmp_2663 ;
       tmp_2663 = tmparr_137[55];
      {
        char* tmp_2664 ;
         tmp_2664 = tmpconstlift_1173;
        tmparr_137[55] = tmp_2664;
      } 
    } 
    {
      char* tmp_2661 ;
       tmp_2661 = tmparr_137[56];
      {
        char* tmp_2662 ;
         tmp_2662 = tmpconstlift_1172;
        tmparr_137[56] = tmp_2662;
      } 
    } 
    {
      char* tmp_2659 ;
       tmp_2659 = tmparr_137[57];
      {
        char* tmp_2660 ;
         tmp_2660 = tmpconstlift_1171;
        tmparr_137[57] = tmp_2660;
      } 
    } 
    {
      char* tmp_2657 ;
       tmp_2657 = tmparr_137[58];
      {
        char* tmp_2658 ;
         tmp_2658 = tmpconstlift_1170;
        tmparr_137[58] = tmp_2658;
      } 
    } 
    {
      char* tmp_2655 ;
       tmp_2655 = tmparr_137[59];
      {
        char* tmp_2656 ;
         tmp_2656 = tmpconstlift_1169;
        tmparr_137[59] = tmp_2656;
      } 
    } 
    {
      char* tmp_2653 ;
       tmp_2653 = tmparr_137[60];
      {
        char* tmp_2654 ;
         tmp_2654 = tmpconstlift_1168;
        tmparr_137[60] = tmp_2654;
      } 
    } 
    {
      char* tmp_2651 ;
       tmp_2651 = tmparr_137[61];
      {
        char* tmp_2652 ;
         tmp_2652 = tmpconstlift_1167;
        tmparr_137[61] = tmp_2652;
      } 
    } 
    {
      char* tmp_2649 ;
       tmp_2649 = tmparr_137[62];
      {
        char* tmp_2650 ;
         tmp_2650 = tmpconstlift_1166;
        tmparr_137[62] = tmp_2650;
      } 
    } 
    {
      char* tmp_2647 ;
       tmp_2647 = tmparr_137[63];
      {
        char* tmp_2648 ;
         tmp_2648 = tmpconstlift_1165;
        tmparr_137[63] = tmp_2648;
      } 
    } 
    {
      char* tmp_2645 ;
       tmp_2645 = tmparr_137[64];
      {
        char* tmp_2646 ;
         tmp_2646 = tmpconstlift_1164;
        tmparr_137[64] = tmp_2646;
      } 
    } 
    {
      char* tmp_2643 ;
       tmp_2643 = tmparr_137[65];
      {
        char* tmp_2644 ;
         tmp_2644 = tmpconstlift_1163;
        tmparr_137[65] = tmp_2644;
      } 
    } 
    {
      char* tmp_2641 ;
       tmp_2641 = tmparr_137[66];
      {
        char* tmp_2642 ;
         tmp_2642 = tmpconstlift_1162;
        tmparr_137[66] = tmp_2642;
      } 
    } 
    {
      char* tmp_2639 ;
       tmp_2639 = tmparr_137[67];
      {
        char* tmp_2640 ;
         tmp_2640 = tmpconstlift_1161;
        tmparr_137[67] = tmp_2640;
      } 
    } 
    {
      char* tmp_2637 ;
       tmp_2637 = tmparr_137[68];
      {
        char* tmp_2638 ;
         tmp_2638 = tmpconstlift_1160;
        tmparr_137[68] = tmp_2638;
      } 
    } 
    {
      char* tmp_2635 ;
       tmp_2635 = tmparr_137[69];
      {
        char* tmp_2636 ;
         tmp_2636 = tmpconstlift_1159;
        tmparr_137[69] = tmp_2636;
      } 
    } 
    {
      char* tmp_2633 ;
       tmp_2633 = tmparr_137[70];
      {
        char* tmp_2634 ;
         tmp_2634 = tmpconstlift_1158;
        tmparr_137[70] = tmp_2634;
      } 
    } 
    {
      char* tmp_2631 ;
       tmp_2631 = tmparr_137[71];
      {
        char* tmp_2632 ;
         tmp_2632 = tmpconstlift_1157;
        tmparr_137[71] = tmp_2632;
      } 
    } 
    {
      char* tmp_2629 ;
       tmp_2629 = tmparr_137[72];
      {
        char* tmp_2630 ;
         tmp_2630 = tmpconstlift_1156;
        tmparr_137[72] = tmp_2630;
      } 
    } 
    {
      char* tmp_2627 ;
       tmp_2627 = tmparr_137[73];
      {
        char* tmp_2628 ;
         tmp_2628 = tmpconstlift_1155;
        tmparr_137[73] = tmp_2628;
      } 
    } 
    {
      char* tmp_2625 ;
       tmp_2625 = tmparr_137[74];
      {
        char* tmp_2626 ;
         tmp_2626 = tmpconstlift_1154;
        tmparr_137[74] = tmp_2626;
      } 
    } 
    {
      char* tmp_2623 ;
       tmp_2623 = tmparr_137[75];
      {
        char* tmp_2624 ;
         tmp_2624 = tmpconstlift_1153;
        tmparr_137[75] = tmp_2624;
      } 
    } 
    {
      char* tmp_2621 ;
       tmp_2621 = tmparr_137[76];
      {
        char* tmp_2622 ;
         tmp_2622 = tmpconstlift_1152;
        tmparr_137[76] = tmp_2622;
      } 
    } 
    {
      char* tmp_2619 ;
       tmp_2619 = tmparr_137[77];
      {
        char* tmp_2620 ;
         tmp_2620 = tmpconstlift_1151;
        tmparr_137[77] = tmp_2620;
      } 
    } 
    {
      char* tmp_2617 ;
       tmp_2617 = tmparr_137[78];
      {
        char* tmp_2618 ;
         tmp_2618 = tmpconstlift_1150;
        tmparr_137[78] = tmp_2618;
      } 
    } 
    {
      char* tmp_2615 ;
       tmp_2615 = tmparr_137[79];
      {
        char* tmp_2616 ;
         tmp_2616 = tmpconstlift_1149;
        tmparr_137[79] = tmp_2616;
      } 
    } 
    {
      char* tmp_2613 ;
       tmp_2613 = tmparr_137[80];
      {
        char* tmp_2614 ;
         tmp_2614 = tmpconstlift_1148;
        tmparr_137[80] = tmp_2614;
      } 
    } 
    {
      char* tmp_2611 ;
       tmp_2611 = tmparr_137[81];
      {
        char* tmp_2612 ;
         tmp_2612 = tmpconstlift_1147;
        tmparr_137[81] = tmp_2612;
      } 
    } 
    {
      char* tmp_2609 ;
       tmp_2609 = tmparr_137[82];
      {
        char* tmp_2610 ;
         tmp_2610 = tmpconstlift_1146;
        tmparr_137[82] = tmp_2610;
      } 
    } 
    {
      char* tmp_2607 ;
       tmp_2607 = tmparr_137[83];
      {
        char* tmp_2608 ;
         tmp_2608 = tmpconstlift_1145;
        tmparr_137[83] = tmp_2608;
      } 
    } 
    {
      char* tmp_2605 ;
       tmp_2605 = tmparr_137[84];
      {
        char* tmp_2606 ;
         tmp_2606 = tmpconstlift_1144;
        tmparr_137[84] = tmp_2606;
      } 
    } 
    {
      char* tmp_2603 ;
       tmp_2603 = tmparr_137[85];
      {
        char* tmp_2604 ;
         tmp_2604 = tmpconstlift_1143;
        tmparr_137[85] = tmp_2604;
      } 
    } 
    {
      char* tmp_2601 ;
       tmp_2601 = tmparr_137[86];
      {
        char* tmp_2602 ;
         tmp_2602 = tmpconstlift_1142;
        tmparr_137[86] = tmp_2602;
      } 
    } 
    {
      char* tmp_2599 ;
       tmp_2599 = tmparr_137[87];
      {
        char* tmp_2600 ;
         tmp_2600 = tmpconstlift_1141;
        tmparr_137[87] = tmp_2600;
      } 
    } 
    {
      char* tmp_2597 ;
       tmp_2597 = tmparr_137[88];
      {
        char* tmp_2598 ;
         tmp_2598 = tmpconstlift_1140;
        tmparr_137[88] = tmp_2598;
      } 
    } 
    {
      char* tmp_2595 ;
       tmp_2595 = tmparr_137[89];
      {
        char* tmp_2596 ;
         tmp_2596 = tmpconstlift_1139;
        tmparr_137[89] = tmp_2596;
      } 
    } 
    {
      char* tmp_2593 ;
       tmp_2593 = tmparr_137[90];
      {
        char* tmp_2594 ;
         tmp_2594 = tmpconstlift_1138;
        tmparr_137[90] = tmp_2594;
      } 
    } 
    {
      char* tmp_2591 ;
       tmp_2591 = tmparr_137[91];
      {
        char* tmp_2592 ;
         tmp_2592 = tmpconstlift_1137;
        tmparr_137[91] = tmp_2592;
      } 
    } 
    {
      char* tmp_2589 ;
       tmp_2589 = tmparr_137[92];
      {
        char* tmp_2590 ;
         tmp_2590 = tmpconstlift_1136;
        tmparr_137[92] = tmp_2590;
      } 
    } 
    {
      char* tmp_2587 ;
       tmp_2587 = tmparr_137[93];
      {
        char* tmp_2588 ;
         tmp_2588 = tmpconstlift_1135;
        tmparr_137[93] = tmp_2588;
      } 
    } 
    {
      char* tmp_2585 ;
       tmp_2585 = tmparr_137[94];
      {
        char* tmp_2586 ;
         tmp_2586 = tmpconstlift_1134;
        tmparr_137[94] = tmp_2586;
      } 
    } 
    {
      char* tmp_2583 ;
       tmp_2583 = tmparr_137[95];
      {
        char* tmp_2584 ;
         tmp_2584 = tmpconstlift_1133;
        tmparr_137[95] = tmp_2584;
      } 
    } 
    {
      char* tmp_2581 ;
       tmp_2581 = tmparr_137[96];
      {
        char* tmp_2582 ;
         tmp_2582 = tmpconstlift_1132;
        tmparr_137[96] = tmp_2582;
      } 
    } 
    {
      char* tmp_2579 ;
       tmp_2579 = tmparr_137[97];
      {
        char* tmp_2580 ;
         tmp_2580 = tmpconstlift_1131;
        tmparr_137[97] = tmp_2580;
      } 
    } 
    {
      char* tmp_2577 ;
       tmp_2577 = tmparr_137[98];
      {
        char* tmp_2578 ;
         tmp_2578 = tmpconstlift_1130;
        tmparr_137[98] = tmp_2578;
      } 
    } 
    {
      char* tmp_2575 ;
       tmp_2575 = tmparr_137[99];
      {
        char* tmp_2576 ;
         tmp_2576 = tmpconstlift_1129;
        tmparr_137[99] = tmp_2576;
      } 
    } 
    {
      char* tmp_2573 ;
       tmp_2573 = tmparr_137[100];
      {
        char* tmp_2574 ;
         tmp_2574 = tmpconstlift_1128;
        tmparr_137[100] = tmp_2574;
      } 
    } 
    {
      char* tmp_2571 ;
       tmp_2571 = tmparr_137[101];
      {
        char* tmp_2572 ;
         tmp_2572 = tmpconstlift_1127;
        tmparr_137[101] = tmp_2572;
      } 
    } 
    {
      char* tmp_2569 ;
       tmp_2569 = tmparr_137[102];
      {
        char* tmp_2570 ;
         tmp_2570 = tmpconstlift_1126;
        tmparr_137[102] = tmp_2570;
      } 
    } 
    {
      char* tmp_2567 ;
       tmp_2567 = tmparr_137[103];
      {
        char* tmp_2568 ;
         tmp_2568 = tmpconstlift_1125;
        tmparr_137[103] = tmp_2568;
      } 
    } 
    {
      char* tmp_2565 ;
       tmp_2565 = tmparr_137[104];
      {
        char* tmp_2566 ;
         tmp_2566 = tmpconstlift_1124;
        tmparr_137[104] = tmp_2566;
      } 
    } 
    {
      char* tmp_2563 ;
       tmp_2563 = tmparr_137[105];
      {
        char* tmp_2564 ;
         tmp_2564 = tmpconstlift_1123;
        tmparr_137[105] = tmp_2564;
      } 
    } 
    {
      char* tmp_2561 ;
       tmp_2561 = tmparr_137[106];
      {
        char* tmp_2562 ;
         tmp_2562 = tmpconstlift_1122;
        tmparr_137[106] = tmp_2562;
      } 
    } 
    {
      char* tmp_2559 ;
       tmp_2559 = tmparr_137[107];
      {
        char* tmp_2560 ;
         tmp_2560 = tmpconstlift_1121;
        tmparr_137[107] = tmp_2560;
      } 
    } 
    {
      char* tmp_2557 ;
       tmp_2557 = tmparr_137[108];
      {
        char* tmp_2558 ;
         tmp_2558 = tmpconstlift_1120;
        tmparr_137[108] = tmp_2558;
      } 
    } 
    {
      char* tmp_2555 ;
       tmp_2555 = tmparr_137[109];
      {
        char* tmp_2556 ;
         tmp_2556 = tmpconstlift_1119;
        tmparr_137[109] = tmp_2556;
      } 
    } 
    {
      char* tmp_2553 ;
       tmp_2553 = tmparr_137[110];
      {
        char* tmp_2554 ;
         tmp_2554 = tmpconstlift_1118;
        tmparr_137[110] = tmp_2554;
      } 
    } 
    {
      char* tmp_2551 ;
       tmp_2551 = tmparr_137[111];
      {
        char* tmp_2552 ;
         tmp_2552 = tmpconstlift_1117;
        tmparr_137[111] = tmp_2552;
      } 
    } 
    {
      char* tmp_2549 ;
       tmp_2549 = tmparr_137[112];
      {
        char* tmp_2550 ;
         tmp_2550 = tmpconstlift_1116;
        tmparr_137[112] = tmp_2550;
      } 
    } 
    {
      char* tmp_2547 ;
       tmp_2547 = tmparr_137[113];
      {
        char* tmp_2548 ;
         tmp_2548 = tmpconstlift_1115;
        tmparr_137[113] = tmp_2548;
      } 
    } 
    {
      char* tmp_2545 ;
       tmp_2545 = tmparr_137[114];
      {
        char* tmp_2546 ;
         tmp_2546 = tmpconstlift_1114;
        tmparr_137[114] = tmp_2546;
      } 
    } 
    {
      char* tmp_2543 ;
       tmp_2543 = tmparr_137[115];
      {
        char* tmp_2544 ;
         tmp_2544 = tmpconstlift_1113;
        tmparr_137[115] = tmp_2544;
      } 
    } 
    {
      char* tmp_2541 ;
       tmp_2541 = tmparr_137[116];
      {
        char* tmp_2542 ;
         tmp_2542 = tmpconstlift_1112;
        tmparr_137[116] = tmp_2542;
      } 
    } 
    {
      char* tmp_2539 ;
       tmp_2539 = tmparr_137[117];
      {
        char* tmp_2540 ;
         tmp_2540 = tmpconstlift_1111;
        tmparr_137[117] = tmp_2540;
      } 
    } 
    {
      char* tmp_2537 ;
       tmp_2537 = tmparr_137[118];
      {
        char* tmp_2538 ;
         tmp_2538 = tmpconstlift_1110;
        tmparr_137[118] = tmp_2538;
      } 
    } 
    {
      char* tmp_2535 ;
       tmp_2535 = tmparr_137[119];
      {
        char* tmp_2536 ;
         tmp_2536 = tmpconstlift_1109;
        tmparr_137[119] = tmp_2536;
      } 
    } 
    {
      char* tmp_2533 ;
       tmp_2533 = tmparr_137[120];
      {
        char* tmp_2534 ;
         tmp_2534 = tmpconstlift_1108;
        tmparr_137[120] = tmp_2534;
      } 
    } 
    {
      char* tmp_2531 ;
       tmp_2531 = tmparr_137[121];
      {
        char* tmp_2532 ;
         tmp_2532 = tmpconstlift_1107;
        tmparr_137[121] = tmp_2532;
      } 
    } 
    {
      char* tmp_2529 ;
       tmp_2529 = tmparr_137[122];
      {
        char* tmp_2530 ;
         tmp_2530 = tmpconstlift_1106;
        tmparr_137[122] = tmp_2530;
      } 
    } 
    {
      char* tmp_2527 ;
       tmp_2527 = tmparr_137[123];
      {
        char* tmp_2528 ;
         tmp_2528 = tmpconstlift_1105;
        tmparr_137[123] = tmp_2528;
      } 
    } 
    {
      char* tmp_2525 ;
       tmp_2525 = tmparr_137[124];
      {
        char* tmp_2526 ;
         tmp_2526 = tmpconstlift_1104;
        tmparr_137[124] = tmp_2526;
      } 
    } 
    {
      char* tmp_2523 ;
       tmp_2523 = tmparr_137[125];
      {
        char* tmp_2524 ;
         tmp_2524 = tmpconstlift_1103;
        tmparr_137[125] = tmp_2524;
      } 
    } 
    {
      char* tmp_2521 ;
       tmp_2521 = tmparr_137[126];
      {
        char* tmp_2522 ;
         tmp_2522 = tmpconstlift_1102;
        tmparr_137[126] = tmp_2522;
      } 
    } 
    {
      char* tmp_2519 ;
       tmp_2519 = tmparr_137[127];
      {
        char* tmp_2520 ;
         tmp_2520 = tmpconstlift_1101;
        tmparr_137[127] = tmp_2520;
      } 
    } 
    {
      char* tmp_2517 ;
       tmp_2517 = tmparr_137[128];
      {
        char* tmp_2518 ;
         tmp_2518 = tmpconstlift_1100;
        tmparr_137[128] = tmp_2518;
      } 
    } 
    {
      char* tmp_2515 ;
       tmp_2515 = tmparr_137[129];
      {
        char* tmp_2516 ;
         tmp_2516 = tmpconstlift_1099;
        tmparr_137[129] = tmp_2516;
      } 
    } 
    {
      char* tmp_2513 ;
       tmp_2513 = tmparr_137[130];
      {
        char* tmp_2514 ;
         tmp_2514 = tmpconstlift_1098;
        tmparr_137[130] = tmp_2514;
      } 
    } 
    {
      char* tmp_2511 ;
       tmp_2511 = tmparr_137[131];
      {
        char* tmp_2512 ;
         tmp_2512 = tmpconstlift_1097;
        tmparr_137[131] = tmp_2512;
      } 
    } 
    {
      char* tmp_2509 ;
       tmp_2509 = tmparr_137[132];
      {
        char* tmp_2510 ;
         tmp_2510 = tmpconstlift_1096;
        tmparr_137[132] = tmp_2510;
      } 
    } 
    {
      char* tmp_2507 ;
       tmp_2507 = tmparr_137[133];
      {
        char* tmp_2508 ;
         tmp_2508 = tmpconstlift_1095;
        tmparr_137[133] = tmp_2508;
      } 
    } 
    {
      char* tmp_2505 ;
       tmp_2505 = tmparr_137[134];
      {
        char* tmp_2506 ;
         tmp_2506 = tmpconstlift_1094;
        tmparr_137[134] = tmp_2506;
      } 
    } 
    {
      char* tmp_2503 ;
       tmp_2503 = tmparr_137[135];
      {
        char* tmp_2504 ;
         tmp_2504 = tmpconstlift_1093;
        tmparr_137[135] = tmp_2504;
      } 
    } 
    {
      char* tmp_2501 ;
       tmp_2501 = tmparr_137[136];
      {
        char* tmp_2502 ;
         tmp_2502 = tmpconstlift_1092;
        tmparr_137[136] = tmp_2502;
      } 
    } 
    {
      char* tmp_2499 ;
       tmp_2499 = tmparr_137[137];
      {
        char* tmp_2500 ;
         tmp_2500 = tmpconstlift_1091;
        tmparr_137[137] = tmp_2500;
      } 
    } 
    {
      char* tmp_2497 ;
       tmp_2497 = tmparr_137[138];
      {
        char* tmp_2498 ;
         tmp_2498 = tmpconstlift_1090;
        tmparr_137[138] = tmp_2498;
      } 
    } 
    {
      char* tmp_2495 ;
       tmp_2495 = tmparr_137[139];
      {
        char* tmp_2496 ;
         tmp_2496 = tmpconstlift_1089;
        tmparr_137[139] = tmp_2496;
      } 
    } 
    {
      char* tmp_2493 ;
       tmp_2493 = tmparr_137[140];
      {
        char* tmp_2494 ;
         tmp_2494 = tmpconstlift_1088;
        tmparr_137[140] = tmp_2494;
      } 
    } 
    {
      char* tmp_2491 ;
       tmp_2491 = tmparr_137[141];
      {
        char* tmp_2492 ;
         tmp_2492 = tmpconstlift_1087;
        tmparr_137[141] = tmp_2492;
      } 
    } 
    {
      char* tmp_2489 ;
       tmp_2489 = tmparr_137[142];
      {
        char* tmp_2490 ;
         tmp_2490 = tmpconstlift_1086;
        tmparr_137[142] = tmp_2490;
      } 
    } 
    {
      char* tmp_2487 ;
       tmp_2487 = tmparr_137[143];
      {
        char* tmp_2488 ;
         tmp_2488 = tmpconstlift_1085;
        tmparr_137[143] = tmp_2488;
      } 
    } 
    {
      char* tmp_2485 ;
       tmp_2485 = tmparr_137[144];
      {
        char* tmp_2486 ;
         tmp_2486 = tmpconstlift_1084;
        tmparr_137[144] = tmp_2486;
      } 
    } 
    {
      char* tmp_2483 ;
       tmp_2483 = tmparr_137[145];
      {
        char* tmp_2484 ;
         tmp_2484 = tmpconstlift_1083;
        tmparr_137[145] = tmp_2484;
      } 
    } 
    {
      char* tmp_2481 ;
       tmp_2481 = tmparr_137[146];
      {
        char* tmp_2482 ;
         tmp_2482 = tmpconstlift_1082;
        tmparr_137[146] = tmp_2482;
      } 
    } 
    {
      char* tmp_2479 ;
       tmp_2479 = tmparr_137[147];
      {
        char* tmp_2480 ;
         tmp_2480 = tmpconstlift_1081;
        tmparr_137[147] = tmp_2480;
      } 
    } 
    {
      char* tmp_2477 ;
       tmp_2477 = tmparr_137[148];
      {
        char* tmp_2478 ;
         tmp_2478 = tmpconstlift_1080;
        tmparr_137[148] = tmp_2478;
      } 
    } 
    {
      char* tmp_2475 ;
       tmp_2475 = tmparr_137[149];
      {
        char* tmp_2476 ;
         tmp_2476 = tmpconstlift_1079;
        tmparr_137[149] = tmp_2476;
      } 
    } 
    {
      char* tmp_2473 ;
       tmp_2473 = tmparr_137[150];
      {
        char* tmp_2474 ;
         tmp_2474 = tmpconstlift_1078;
        tmparr_137[150] = tmp_2474;
      } 
    } 
    {
      char* tmp_2471 ;
       tmp_2471 = tmparr_137[151];
      {
        char* tmp_2472 ;
         tmp_2472 = tmpconstlift_1077;
        tmparr_137[151] = tmp_2472;
      } 
    } 
    {
      char* tmp_2469 ;
       tmp_2469 = tmparr_137[152];
      {
        char* tmp_2470 ;
         tmp_2470 = tmpconstlift_1076;
        tmparr_137[152] = tmp_2470;
      } 
    } 
    {
      char* tmp_2467 ;
       tmp_2467 = tmparr_137[153];
      {
        char* tmp_2468 ;
         tmp_2468 = tmpconstlift_1075;
        tmparr_137[153] = tmp_2468;
      } 
    } 
    {
      char* tmp_2465 ;
       tmp_2465 = tmparr_137[154];
      {
        char* tmp_2466 ;
         tmp_2466 = tmpconstlift_1074;
        tmparr_137[154] = tmp_2466;
      } 
    } 
    {
      char* tmp_2463 ;
       tmp_2463 = tmparr_137[155];
      {
        char* tmp_2464 ;
         tmp_2464 = tmpconstlift_1073;
        tmparr_137[155] = tmp_2464;
      } 
    } 
    {
      char* tmp_2461 ;
       tmp_2461 = tmparr_137[156];
      {
        char* tmp_2462 ;
         tmp_2462 = tmpconstlift_1072;
        tmparr_137[156] = tmp_2462;
      } 
    } 
    {
      char* tmp_2459 ;
       tmp_2459 = tmparr_137[157];
      {
        char* tmp_2460 ;
         tmp_2460 = tmpconstlift_1071;
        tmparr_137[157] = tmp_2460;
      } 
    } 
    {
      char* tmp_2457 ;
       tmp_2457 = tmparr_137[158];
      {
        char* tmp_2458 ;
         tmp_2458 = tmpconstlift_1070;
        tmparr_137[158] = tmp_2458;
      } 
    } 
    {
      char* tmp_2455 ;
       tmp_2455 = tmparr_137[159];
      {
        char* tmp_2456 ;
         tmp_2456 = tmpconstlift_1069;
        tmparr_137[159] = tmp_2456;
      } 
    } 
    {
      char* tmp_2453 ;
       tmp_2453 = tmparr_137[160];
      {
        char* tmp_2454 ;
         tmp_2454 = tmpconstlift_1068;
        tmparr_137[160] = tmp_2454;
      } 
    } 
    {
      char* tmp_2451 ;
       tmp_2451 = tmparr_137[161];
      {
        char* tmp_2452 ;
         tmp_2452 = tmpconstlift_1067;
        tmparr_137[161] = tmp_2452;
      } 
    } 
    {
      char* tmp_2449 ;
       tmp_2449 = tmparr_137[162];
      {
        char* tmp_2450 ;
         tmp_2450 = tmpconstlift_1066;
        tmparr_137[162] = tmp_2450;
      } 
    } 
    {
      char* tmp_2447 ;
       tmp_2447 = tmparr_137[163];
      {
        char* tmp_2448 ;
         tmp_2448 = tmpconstlift_1065;
        tmparr_137[163] = tmp_2448;
      } 
    } 
    {
      char* tmp_2445 ;
       tmp_2445 = tmparr_137[164];
      {
        char* tmp_2446 ;
         tmp_2446 = tmpconstlift_1064;
        tmparr_137[164] = tmp_2446;
      } 
    } 
    {
      char* tmp_2443 ;
       tmp_2443 = tmparr_137[165];
      {
        char* tmp_2444 ;
         tmp_2444 = tmpconstlift_1063;
        tmparr_137[165] = tmp_2444;
      } 
    } 
    {
      char* tmp_2441 ;
       tmp_2441 = tmparr_137[166];
      {
        char* tmp_2442 ;
         tmp_2442 = tmpconstlift_1062;
        tmparr_137[166] = tmp_2442;
      } 
    } 
    {
      char* tmp_2439 ;
       tmp_2439 = tmparr_137[167];
      {
        char* tmp_2440 ;
         tmp_2440 = tmpconstlift_1061;
        tmparr_137[167] = tmp_2440;
      } 
    } 
    {
      char* tmp_2437 ;
       tmp_2437 = tmparr_137[168];
      {
        char* tmp_2438 ;
         tmp_2438 = tmpconstlift_1060;
        tmparr_137[168] = tmp_2438;
      } 
    } 
    {
      char* tmp_2435 ;
       tmp_2435 = tmparr_137[169];
      {
        char* tmp_2436 ;
         tmp_2436 = tmpconstlift_1059;
        tmparr_137[169] = tmp_2436;
      } 
    } 
    {
      char* tmp_2433 ;
       tmp_2433 = tmparr_137[170];
      {
        char* tmp_2434 ;
         tmp_2434 = tmpconstlift_1058;
        tmparr_137[170] = tmp_2434;
      } 
    } 
    {
      char* tmp_2431 ;
       tmp_2431 = tmparr_137[171];
      {
        char* tmp_2432 ;
         tmp_2432 = tmpconstlift_1057;
        tmparr_137[171] = tmp_2432;
      } 
    } 
    {
      char* tmp_2429 ;
       tmp_2429 = tmparr_137[172];
      {
        char* tmp_2430 ;
         tmp_2430 = tmpconstlift_1056;
        tmparr_137[172] = tmp_2430;
      } 
    } 
    {
      char* tmp_2427 ;
       tmp_2427 = tmparr_137[173];
      {
        char* tmp_2428 ;
         tmp_2428 = tmpconstlift_1055;
        tmparr_137[173] = tmp_2428;
      } 
    } 
    {
      char* tmp_2425 ;
       tmp_2425 = tmparr_137[174];
      {
        char* tmp_2426 ;
         tmp_2426 = tmpconstlift_1054;
        tmparr_137[174] = tmp_2426;
      } 
    } 
    {
      char* tmp_2423 ;
       tmp_2423 = tmparr_137[175];
      {
        char* tmp_2424 ;
         tmp_2424 = tmpconstlift_1053;
        tmparr_137[175] = tmp_2424;
      } 
    } 
    {
      char* tmp_2421 ;
       tmp_2421 = tmparr_137[176];
      {
        char* tmp_2422 ;
         tmp_2422 = tmpconstlift_1052;
        tmparr_137[176] = tmp_2422;
      } 
    } 
    {
      char* tmp_2419 ;
       tmp_2419 = tmparr_137[177];
      {
        char* tmp_2420 ;
         tmp_2420 = tmpconstlift_1051;
        tmparr_137[177] = tmp_2420;
      } 
    } 
    {
      char* tmp_2417 ;
       tmp_2417 = tmparr_137[178];
      {
        char* tmp_2418 ;
         tmp_2418 = tmpconstlift_1050;
        tmparr_137[178] = tmp_2418;
      } 
    } 
    {
      char* tmp_2415 ;
       tmp_2415 = tmparr_137[179];
      {
        char* tmp_2416 ;
         tmp_2416 = tmpconstlift_1049;
        tmparr_137[179] = tmp_2416;
      } 
    } 
    {
      char* tmp_2413 ;
       tmp_2413 = tmparr_137[180];
      {
        char* tmp_2414 ;
         tmp_2414 = tmpconstlift_1048;
        tmparr_137[180] = tmp_2414;
      } 
    } 
    {
      char* tmp_2411 ;
       tmp_2411 = tmparr_137[181];
      {
        char* tmp_2412 ;
         tmp_2412 = tmpconstlift_1047;
        tmparr_137[181] = tmp_2412;
      } 
    } 
    {
      char* tmp_2409 ;
       tmp_2409 = tmparr_137[182];
      {
        char* tmp_2410 ;
         tmp_2410 = tmpconstlift_1046;
        tmparr_137[182] = tmp_2410;
      } 
    } 
    {
      char* tmp_2407 ;
       tmp_2407 = tmparr_137[183];
      {
        char* tmp_2408 ;
         tmp_2408 = tmpconstlift_1045;
        tmparr_137[183] = tmp_2408;
      } 
    } 
    {
      char* tmp_2405 ;
       tmp_2405 = tmparr_137[184];
      {
        char* tmp_2406 ;
         tmp_2406 = tmpconstlift_1044;
        tmparr_137[184] = tmp_2406;
      } 
    } 
    {
      char* tmp_2403 ;
       tmp_2403 = tmparr_137[185];
      {
        char* tmp_2404 ;
         tmp_2404 = tmpconstlift_1043;
        tmparr_137[185] = tmp_2404;
      } 
    } 
    {
      char* tmp_2401 ;
       tmp_2401 = tmparr_137[186];
      {
        char* tmp_2402 ;
         tmp_2402 = tmpconstlift_1042;
        tmparr_137[186] = tmp_2402;
      } 
    } 
    {
      char* tmp_2399 ;
       tmp_2399 = tmparr_137[187];
      {
        char* tmp_2400 ;
         tmp_2400 = tmpconstlift_1041;
        tmparr_137[187] = tmp_2400;
      } 
    } 
    {
      char* tmp_2397 ;
       tmp_2397 = tmparr_137[188];
      {
        char* tmp_2398 ;
         tmp_2398 = tmpconstlift_1040;
        tmparr_137[188] = tmp_2398;
      } 
    } 
    {
      char* tmp_2395 ;
       tmp_2395 = tmparr_137[189];
      {
        char* tmp_2396 ;
         tmp_2396 = tmpconstlift_1039;
        tmparr_137[189] = tmp_2396;
      } 
    } 
    {
      char* tmp_2393 ;
       tmp_2393 = tmparr_137[190];
      {
        char* tmp_2394 ;
         tmp_2394 = tmpconstlift_1038;
        tmparr_137[190] = tmp_2394;
      } 
    } 
    {
      char* tmp_2391 ;
       tmp_2391 = tmparr_137[191];
      {
        char* tmp_2392 ;
         tmp_2392 = tmpconstlift_1037;
        tmparr_137[191] = tmp_2392;
      } 
    } 
    {
      char* tmp_2389 ;
       tmp_2389 = tmparr_137[192];
      {
        char* tmp_2390 ;
         tmp_2390 = tmpconstlift_1036;
        tmparr_137[192] = tmp_2390;
      } 
    } 
    {
      char* tmp_2387 ;
       tmp_2387 = tmparr_137[193];
      {
        char* tmp_2388 ;
         tmp_2388 = tmpconstlift_1035;
        tmparr_137[193] = tmp_2388;
      } 
    } 
    {
      char* tmp_2385 ;
       tmp_2385 = tmparr_137[194];
      {
        char* tmp_2386 ;
         tmp_2386 = tmpconstlift_1034;
        tmparr_137[194] = tmp_2386;
      } 
    } 
    {
      char* tmp_2383 ;
       tmp_2383 = tmparr_137[195];
      {
        char* tmp_2384 ;
         tmp_2384 = tmpconstlift_1033;
        tmparr_137[195] = tmp_2384;
      } 
    } 
    {
      char* tmp_2381 ;
       tmp_2381 = tmparr_137[196];
      {
        char* tmp_2382 ;
         tmp_2382 = tmpconstlift_1032;
        tmparr_137[196] = tmp_2382;
      } 
    } 
    {
      char* tmp_2379 ;
       tmp_2379 = tmparr_137[197];
      {
        char* tmp_2380 ;
         tmp_2380 = tmpconstlift_1031;
        tmparr_137[197] = tmp_2380;
      } 
    } 
    {
      char* tmp_2377 ;
       tmp_2377 = tmparr_137[198];
      {
        char* tmp_2378 ;
         tmp_2378 = tmpconstlift_1030;
        tmparr_137[198] = tmp_2378;
      } 
    } 
    {
      char* tmp_2375 ;
       tmp_2375 = tmparr_137[199];
      {
        char* tmp_2376 ;
         tmp_2376 = tmpconstlift_1029;
        tmparr_137[199] = tmp_2376;
      } 
    } 
    {
      char* tmp_2373 ;
       tmp_2373 = tmparr_137[200];
      {
        char* tmp_2374 ;
         tmp_2374 = tmpconstlift_1028;
        tmparr_137[200] = tmp_2374;
      } 
    } 
    {
      char* tmp_2371 ;
       tmp_2371 = tmparr_137[201];
      {
        char* tmp_2372 ;
         tmp_2372 = tmpconstlift_1027;
        tmparr_137[201] = tmp_2372;
      } 
    } 
    {
      char* tmp_2369 ;
       tmp_2369 = tmparr_137[202];
      {
        char* tmp_2370 ;
         tmp_2370 = tmpconstlift_1026;
        tmparr_137[202] = tmp_2370;
      } 
    } 
    {
      char* tmp_2367 ;
       tmp_2367 = tmparr_137[203];
      {
        char* tmp_2368 ;
         tmp_2368 = tmpconstlift_1025;
        tmparr_137[203] = tmp_2368;
      } 
    } 
    {
      char* tmp_2365 ;
       tmp_2365 = tmparr_137[204];
      {
        char* tmp_2366 ;
         tmp_2366 = tmpconstlift_1024;
        tmparr_137[204] = tmp_2366;
      } 
    } 
    {
      char* tmp_2363 ;
       tmp_2363 = tmparr_137[205];
      {
        char* tmp_2364 ;
         tmp_2364 = tmpconstlift_1023;
        tmparr_137[205] = tmp_2364;
      } 
    } 
    {
      char* tmp_2361 ;
       tmp_2361 = tmparr_137[206];
      {
        char* tmp_2362 ;
         tmp_2362 = tmpconstlift_1022;
        tmparr_137[206] = tmp_2362;
      } 
    } 
    {
      char* tmp_2359 ;
       tmp_2359 = tmparr_137[207];
      {
        char* tmp_2360 ;
         tmp_2360 = tmpconstlift_1021;
        tmparr_137[207] = tmp_2360;
      } 
    } 
    {
      char* tmp_2357 ;
       tmp_2357 = tmparr_137[208];
      {
        char* tmp_2358 ;
         tmp_2358 = tmpconstlift_1020;
        tmparr_137[208] = tmp_2358;
      } 
    } 
    {
      char* tmp_2355 ;
       tmp_2355 = tmparr_137[209];
      {
        char* tmp_2356 ;
         tmp_2356 = tmpconstlift_1019;
        tmparr_137[209] = tmp_2356;
      } 
    } 
    {
      char* tmp_2353 ;
       tmp_2353 = tmparr_137[210];
      {
        char* tmp_2354 ;
         tmp_2354 = tmpconstlift_1018;
        tmparr_137[210] = tmp_2354;
      } 
    } 
    {
      char* tmp_2351 ;
       tmp_2351 = tmparr_137[211];
      {
        char* tmp_2352 ;
         tmp_2352 = tmpconstlift_1017;
        tmparr_137[211] = tmp_2352;
      } 
    } 
    {
      char* tmp_2349 ;
       tmp_2349 = tmparr_137[212];
      {
        char* tmp_2350 ;
         tmp_2350 = tmpconstlift_1016;
        tmparr_137[212] = tmp_2350;
      } 
    } 
    {
      char* tmp_2347 ;
       tmp_2347 = tmparr_137[213];
      {
        char* tmp_2348 ;
         tmp_2348 = tmpconstlift_1015;
        tmparr_137[213] = tmp_2348;
      } 
    } 
    {
      char* tmp_2345 ;
       tmp_2345 = tmparr_137[214];
      {
        char* tmp_2346 ;
         tmp_2346 = tmpconstlift_1014;
        tmparr_137[214] = tmp_2346;
      } 
    } 
    {
      char* tmp_2343 ;
       tmp_2343 = tmparr_137[215];
      {
        char* tmp_2344 ;
         tmp_2344 = tmpconstlift_1013;
        tmparr_137[215] = tmp_2344;
      } 
    } 
    {
      char* tmp_2341 ;
       tmp_2341 = tmparr_137[216];
      {
        char* tmp_2342 ;
         tmp_2342 = tmpconstlift_1012;
        tmparr_137[216] = tmp_2342;
      } 
    } 
    {
      char* tmp_2339 ;
       tmp_2339 = tmparr_137[217];
      {
        char* tmp_2340 ;
         tmp_2340 = tmpconstlift_1011;
        tmparr_137[217] = tmp_2340;
      } 
    } 
    {
      char* tmp_2337 ;
       tmp_2337 = tmparr_137[218];
      {
        char* tmp_2338 ;
         tmp_2338 = tmpconstlift_1010;
        tmparr_137[218] = tmp_2338;
      } 
    } 
    {
      char* tmp_2335 ;
       tmp_2335 = tmparr_137[219];
      {
        char* tmp_2336 ;
         tmp_2336 = tmpconstlift_1009;
        tmparr_137[219] = tmp_2336;
      } 
    } 
    {
      char* tmp_2333 ;
       tmp_2333 = tmparr_137[220];
      {
        char* tmp_2334 ;
         tmp_2334 = tmpconstlift_1008;
        tmparr_137[220] = tmp_2334;
      } 
    } 
    {
      char* tmp_2331 ;
       tmp_2331 = tmparr_137[221];
      {
        char* tmp_2332 ;
         tmp_2332 = tmpconstlift_1007;
        tmparr_137[221] = tmp_2332;
      } 
    } 
    {
      char* tmp_2329 ;
       tmp_2329 = tmparr_137[222];
      {
        char* tmp_2330 ;
         tmp_2330 = tmpconstlift_1006;
        tmparr_137[222] = tmp_2330;
      } 
    } 
    {
      char* tmp_2327 ;
       tmp_2327 = tmparr_137[223];
      {
        char* tmp_2328 ;
         tmp_2328 = tmpconstlift_1005;
        tmparr_137[223] = tmp_2328;
      } 
    } 
    {
      char* tmp_2325 ;
       tmp_2325 = tmparr_137[224];
      {
        char* tmp_2326 ;
         tmp_2326 = tmpconstlift_1004;
        tmparr_137[224] = tmp_2326;
      } 
    } 
    {
      char* tmp_2323 ;
       tmp_2323 = tmparr_137[225];
      {
        char* tmp_2324 ;
         tmp_2324 = tmpconstlift_1003;
        tmparr_137[225] = tmp_2324;
      } 
    } 
    {
      char* tmp_2321 ;
       tmp_2321 = tmparr_137[226];
      {
        char* tmp_2322 ;
         tmp_2322 = tmpconstlift_1002;
        tmparr_137[226] = tmp_2322;
      } 
    } 
    {
      char* tmp_2319 ;
       tmp_2319 = tmparr_137[227];
      {
        char* tmp_2320 ;
         tmp_2320 = tmpconstlift_1001;
        tmparr_137[227] = tmp_2320;
      } 
    } 
    {
      char* tmp_2317 ;
       tmp_2317 = tmparr_137[228];
      {
        char* tmp_2318 ;
         tmp_2318 = tmpconstlift_1000;
        tmparr_137[228] = tmp_2318;
      } 
    } 
    {
      char* tmp_2315 ;
       tmp_2315 = tmparr_137[229];
      {
        char* tmp_2316 ;
         tmp_2316 = tmpconstlift_999;
        tmparr_137[229] = tmp_2316;
      } 
    } 
    {
      char* tmp_2313 ;
       tmp_2313 = tmparr_137[230];
      {
        char* tmp_2314 ;
         tmp_2314 = tmpconstlift_998;
        tmparr_137[230] = tmp_2314;
      } 
    } 
    {
      char* tmp_2311 ;
       tmp_2311 = tmparr_137[231];
      {
        char* tmp_2312 ;
         tmp_2312 = tmpconstlift_997;
        tmparr_137[231] = tmp_2312;
      } 
    } 
    {
      char* tmp_2309 ;
       tmp_2309 = tmparr_137[232];
      {
        char* tmp_2310 ;
         tmp_2310 = tmpconstlift_996;
        tmparr_137[232] = tmp_2310;
      } 
    } 
    {
      char* tmp_2307 ;
       tmp_2307 = tmparr_137[233];
      {
        char* tmp_2308 ;
         tmp_2308 = tmpconstlift_995;
        tmparr_137[233] = tmp_2308;
      } 
    } 
    {
      char* tmp_2305 ;
       tmp_2305 = tmparr_137[234];
      {
        char* tmp_2306 ;
         tmp_2306 = tmpconstlift_994;
        tmparr_137[234] = tmp_2306;
      } 
    } 
    {
      char* tmp_2303 ;
       tmp_2303 = tmparr_137[235];
      {
        char* tmp_2304 ;
         tmp_2304 = tmpconstlift_993;
        tmparr_137[235] = tmp_2304;
      } 
    } 
    {
      char* tmp_2301 ;
       tmp_2301 = tmparr_137[236];
      {
        char* tmp_2302 ;
         tmp_2302 = tmpconstlift_992;
        tmparr_137[236] = tmp_2302;
      } 
    } 
    {
      char* tmp_2299 ;
       tmp_2299 = tmparr_137[237];
      {
        char* tmp_2300 ;
         tmp_2300 = tmpconstlift_991;
        tmparr_137[237] = tmp_2300;
      } 
    } 
    {
      char* tmp_2297 ;
       tmp_2297 = tmparr_137[238];
      {
        char* tmp_2298 ;
         tmp_2298 = tmpconstlift_990;
        tmparr_137[238] = tmp_2298;
      } 
    } 
    {
      char* tmp_2295 ;
       tmp_2295 = tmparr_137[239];
      {
        char* tmp_2296 ;
         tmp_2296 = tmpconstlift_989;
        tmparr_137[239] = tmp_2296;
      } 
    } 
    {
      char* tmp_2293 ;
       tmp_2293 = tmparr_137[240];
      {
        char* tmp_2294 ;
         tmp_2294 = tmpconstlift_988;
        tmparr_137[240] = tmp_2294;
      } 
    } 
    {
      char* tmp_2291 ;
       tmp_2291 = tmparr_137[241];
      {
        char* tmp_2292 ;
         tmp_2292 = tmpconstlift_987;
        tmparr_137[241] = tmp_2292;
      } 
    } 
    {
      char* tmp_2289 ;
       tmp_2289 = tmparr_137[242];
      {
        char* tmp_2290 ;
         tmp_2290 = tmpconstlift_986;
        tmparr_137[242] = tmp_2290;
      } 
    } 
    {
      char* tmp_2287 ;
       tmp_2287 = tmparr_137[243];
      {
        char* tmp_2288 ;
         tmp_2288 = tmpconstlift_985;
        tmparr_137[243] = tmp_2288;
      } 
    } 
    {
      char* tmp_2285 ;
       tmp_2285 = tmparr_137[244];
      {
        char* tmp_2286 ;
         tmp_2286 = tmpconstlift_984;
        tmparr_137[244] = tmp_2286;
      } 
    } 
    {
      char* tmp_2283 ;
       tmp_2283 = tmparr_137[245];
      {
        char* tmp_2284 ;
         tmp_2284 = tmpconstlift_983;
        tmparr_137[245] = tmp_2284;
      } 
    } 
    {
      char* tmp_2281 ;
       tmp_2281 = tmparr_137[246];
      {
        char* tmp_2282 ;
         tmp_2282 = tmpconstlift_982;
        tmparr_137[246] = tmp_2282;
      } 
    } 
    {
      char* tmp_2279 ;
       tmp_2279 = tmparr_137[247];
      {
        char* tmp_2280 ;
         tmp_2280 = tmpconstlift_981;
        tmparr_137[247] = tmp_2280;
      } 
    } 
     all_syms_82 = tmparr_137;
  } 
  {
    float tmprc_2273 ;
     tmprc_2273 = 0.0F;
     t_81 = tmprc_2273;
  } 
   spawn_socket_client_helper_90 = 0;
   wsexit_111 = 0;
  {
    uint8_t* tmprc_2795 ;
    int* arrtmp_3701 = (int*)0;
    if (4 > 0) {
      arrtmp_3701 = (int*)((char*)WSCALLOC_SCALAR((sizeof(uint8_t) * 4) + RCSIZE + ARRLENSIZE, 1) + RCSIZE + ARRLENSIZE);
      SETARRLEN(arrtmp_3701, 4);
    } 
     tmprc_2795 = (uint8_t*)arrtmp_3701;
     lenbuf_110 = tmprc_2795;
  } 
   Unix_read_bytes_offset_109 = 0;
   Unix_puts_err_108 = 0;
   Unix_get_errno_107 = 0;
   shutdown_sockets_105 = 0;
   Unix_ewouldblock_104 = 0;
   poll_socket_client_ready_port_103 = 0;
  {
    int tmprc_2794 ;
     tmprc_2794 = 0;
     curstate_102 = tmprc_2794;
  } 
  {
    uint8_t* tmprc_2793 ;
     tmprc_2793 = ((uint8_t*)0) /* Array:null */;
     databuf_101 = tmprc_2793;
  } 
  {
    int tmprc_2792 ;
     tmprc_2792 = 0;
     msglen_100 = tmprc_2792;
  } 
  {
    int tmprc_2791 ;
     tmprc_2791 = 0;
     wouldblock_99 = tmprc_2791;
  } 
  {
    int tmprc_2790 ;
     tmprc_2790 = 0;
     datactr_98 = tmprc_2790;
  } 
  {
    int tmprc_2789 ;
     tmprc_2789 = 0;
     sockfd_97 = tmprc_2789;
  } 
  {
    int tmprc_2788 ;
     tmprc_2788 = 0;
     lenctr_96 = tmprc_2788;
  } 
  {
    ws_bool_t tmprc_2787 ;
     tmprc_2787 = FALSE;
     connected_95 = tmprc_2787;
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
  int counter_wsq_randomSource_8 = 0;
    initState();
  ws_parse_options(argc,argv); /* [2008.08.27] Moving to after initState */ 
  ws_bool_t dummy =TRUE;
  // Insert calls to those timers executing only once (with zero rates)
  GRAB_WRITEFIFO(socket_in_raw_10);
  EMIT(((char)0), char, socket_in_raw_10);
  RELEASE_WRITEFIFO(socket_in_raw_10);
  // Next, run in a timer loop indefinitely
  while(dummy && !stopalltimers) {
    counter_wsq_randomSource_8++;
    VIRTTICK();
    int fired = 0;
    // And finally call the normal-timers on ticks where it is appropriate:
    if (counter_wsq_randomSource_8 == 1) {
      GRAB_WRITEFIFO(wsq_randomSource_6);
      EMIT(((char)0), char, wsq_randomSource_6);
      RELEASE_WRITEFIFO(wsq_randomSource_6);
      counter_wsq_randomSource_8 = 0;
      fired = 1;
    } 
    // The timesteps where we fire are the only 'real' ones:
    if (fired) {
      // Use do-while here to make sure that the negative guys go at least ONCE:
      do {
        // Insert calls to those (negative rate) timers that run max speed:
        // This substitutes for a call to WAIT_TICKS:
        GRAB_WRITEFIFO(socket_in_raw_11);
        EMIT(((char)0), char, socket_in_raw_11);
        RELEASE_WRITEFIFO(socket_in_raw_11);
      } 
      while (TIME_DEBT(1.0) > 0.0);
    } 
  } 
  // We keep the main function going for other tuples to come through.
  while (print_queue_status()) { sleep(1); }
  return 0;
} 
