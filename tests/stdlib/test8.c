
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "../../libcnet/str.h"
#include "../../libcnet/utils.h"
#include "../../libcnet/io.h"

int main()
{
    string **tokens = cnet_str_split("GET /file/path.html HTTP/1.0", " ");
    string *get = cnet_new_str_nolen("GET");
    string *path = cnet_new_str_nolen("/file/path.html");
    string *prot = cnet_new_str_nolen("HTTP/1.0");

    assert(cnet_strcmp(tokens[0], get) == 0);
    assert(cnet_strcmp(tokens[1], path) == 0);
    assert(cnet_strcmp(tokens[2], prot) == 0);
}
