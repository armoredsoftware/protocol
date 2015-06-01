extern char** str_split(char*, const char);
extern void free_str_split(char**);

extern int ME_sock_server_connect (void);
extern int ME_sock_recv(int,char *);
extern void ME_sock_send(int, char *);
extern void ME_sock_recv_dynamic(int, int *, char **);
extern void ME_sock_send_dynamic(int, int, char *);

struct ME_CG;
struct ME_FT;

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
extern int ME_FT_add(struct ME_FT *, char *);
extern void ME_FT_print(struct ME_FT *);
extern int ME_FT_get_index(struct ME_FT *, char *);
extern char * ME_FT_get(struct ME_FT *, int);
extern void ME_FT_encode(struct ME_FT *, int *, char **);
extern void ME_FT_decode(char *, struct ME_FT **);
extern void ME_FT_print_encoded(char *);
