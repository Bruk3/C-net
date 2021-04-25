#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "../../libcnet/str.h"
#include "../../libcnet/utils.h"
#include "../../libcnet/io.h"

int main(){

    cnet_file *file = user_fopen(cnet_new_str_nolen("file.txt"), cnet_new_str_nolen("rb"));
    string *newline = cnet_new_str_nolen("\n");


    /***************** Test cnet_readln  **************************/
    string *line = cnet_readln(file);
    string *temp = cnet_new_str_nolen("hellow");
    string *temp15 = cnet_strmult(temp, 15);
    string *expected = cnet_strcat(temp15, newline);
    assert(cnet_strcmp(line, expected) == 0);
    printf("passed first line test\n");
    temp->cnet_free(temp);
    temp->cnet_free(temp15);
    expected->cnet_free(expected);
    line->cnet_free(line);

    for (int i=0; i < 5; i++) {
        string *line = cnet_readln(file);
        char curr[10];
        sprintf(curr, "line%d", i);
        string *temp = cnet_new_str_nolen(curr);
        assert(cnet_strcmp(line, temp) == 0);
        line->cnet_free(line);
        temp->cnet_free(temp);
    }

    printf("passed consecutive 5 lines test\n");

    // read very long line
    line = cnet_readln(file);
    printf("read length: %d\n", line->length);
    string *a = cnet_new_str_nolen("a");
    string *manyAs = cnet_strmult(a, 4341);
    expected = cnet_strcat(manyAs, newline);
    assert(cnet_strcmp(line, expected) == 0);
    printf("passed long line test\n");
    line->cnet_free(line); a->cnet_free(a);
    expected->cnet_free(expected);
    newline->cnet_free(newline);
    manyAs->cnet_free(manyAs);




    // close file
    fclose(file->f);


    /*********************************************************/


    /***************** Test cnet_nread  **************************/




    // string *fromFile = cnet_nread(file, 90);
    // assert(cnet_strcmp(fromFile, expected));

}