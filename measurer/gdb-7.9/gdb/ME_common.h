extern char** str_split(char*, const char);
extern void free_str_split(char**);

extern int ME_sock_server_connect (void);
extern int ME_sock_recv(int,char *);
extern void ME_sock_send(int, char *);
extern void ME_sock_recv_dynamic(int, int *, char **);
extern void ME_sock_send_dynamic(int, int, char *);

struct ME_CG;
struct ME_FT;

struct ME_CG_AND_FT;

extern struct ME_CG * ME_CG_create(int);
extern void ME_CG_add_child(struct ME_CG *, struct ME_CG *);
extern struct ME_CG * ME_CG_copy(struct ME_CG *);
extern void ME_CG_delete(struct ME_CG *);
extern void ME_CG_merge_stack(struct ME_CG *, struct ME_CG *);
extern void ME_CG_print_s_h(struct ME_CG *);
extern void ME_CG_print_s(struct ME_CG *);
extern void ME_CG_print_h(struct ME_CG *, struct ME_FT *);
extern void ME_CG_print(struct ME_CG *, struct ME_FT *);
extern int ME_CG_count(struct ME_CG *);
extern int ME_CG_encode_h(struct ME_CG *, int, int *);
extern void ME_CG_encode(struct ME_CG *, int *, char **);
extern void ME_CG_print_encoded(int, char *);
extern struct ME_CG * ME_CG_decode_h(int *, int);
extern void ME_CG_decode(char *, struct ME_CG **);

extern struct ME_FT * ME_FT_create(char *);
extern void ME_FT_delete(struct ME_FT *);
extern int ME_FT_add(struct ME_FT *, char *);
extern void ME_FT_print(struct ME_FT *);
extern int ME_FT_get_index(struct ME_FT *, char *);
extern char * ME_FT_get(struct ME_FT *, int);
extern void ME_FT_encode(struct ME_FT *, int *, char **);
extern void ME_FT_decode(char *, struct ME_FT **);
extern void ME_FT_print_encoded(char *);

enum ME_measurement_type;
struct ME_measurement;

extern struct ME_measurement * ME_measurement_create(enum ME_measurement_type);
extern void ME_measurement_delete(struct ME_measurement *);
extern void ME_measurement_print(struct ME_measurement *);
extern void ME_measurement_send(int, struct ME_measurement *);
extern void ME_measurement_send_temp(int, struct ME_measurement *);
extern struct ME_measurement * ME_measurement_recieve(int);

enum ME_feature_type;
struct ME_feature;

enum BE_event_type;
struct BE_event_t;
struct BE_event_b;
struct BE_event;

struct BE_hook;

extern struct ME_feature * ME_feature_create_callstack();
extern struct ME_feature * ME_feature_create_variable(char *);
extern struct ME_feature * ME_feature_create_memory(char *, char *);
extern void ME_feature_print(struct ME_feature *);

extern struct BE_event * BE_event_t_create(int, int);
extern struct BE_event * BE_event_b_create(int, int);
extern void BE_event_print(struct BE_event *);
extern void BE_hook_print(struct BE_hook *);
extern void BE_hook_disable(struct BE_hook *);
extern void BE_hook_enable(struct BE_hook *);
extern void BE_hook_kill(struct BE_hook *);
extern void BE_hook_array_init();
extern int BE_hook_array_add(struct BE_hook *);
extern struct BE_hook * BE_hook_array_get(int);
