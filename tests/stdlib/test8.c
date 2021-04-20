#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "../../libcnet/str.h"
#include "../../libcnet/utils.h"
#include "../../libcnet/io.h"

int main()
{
    string **tokens = mem_alloc(sizeof(string *) * 3);
    string *sep = cnet_new_str_nolen(" ");
    string *reqline = cnet_new_str_nolen("GET /file/path.html HTTP/1.0 This Should not be parsed");
    cnet_str_split(reqline, sep, tokens, 3);
    string *get = cnet_new_str_nolen("GET");
    string *path = cnet_new_str_nolen("/file/path.html");
    string *prot = cnet_new_str_nolen("HTTP/1.0");

    assert(cnet_strcmp(tokens[0], get) == 0);
    assert(cnet_strcmp(tokens[1], path) == 0);
    assert(cnet_strcmp(tokens[2], prot) == 0);
    // print_cnet_str(tokens[0]);
    // print_cnet_str(tokens[1]);
    // print_cnet_str(tokens[2]);

    sep->cnet_free(sep);
    reqline->cnet_free(reqline);
    get->cnet_free(get);
    path->cnet_free(path);
    prot->cnet_free(prot);
    for (int i = 0 ; i < 3; i++){
        (tokens[i])->cnet_free(tokens[i]);
    }
    free(tokens);
}
