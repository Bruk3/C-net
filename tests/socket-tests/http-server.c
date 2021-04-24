void die(string s) { s.write_ln; exit(1); }

int main(string[] argv)
{
    if (argv.slength() != 3){
        fprintf(stderr, "usage: %s <server-port> <web-root>", argv[0]);
        exit(1);
    }

    unsigned short server_port = atoi(argv[1]);
    char *webroot = argv[2];

    // Create a listening socket (also called server socket)
    // for liking with the mdb-lookup server.
    socket listener = nopen(0, 0, argv[1].stoi());

    int clntsock;
    FILE *fp_sock_r, *fp_sock_w, *fp_target;
    char buf[4096];
    while (1) {

        // Accept an incoming connection

        clntlen = sizeof(clntaddr); // initialize the in-out parameter
	    socket clntsock = naccept(listener);
        /*
        // accept() returned a connected socket (also called client socket)
        // and filled in the client's address into clntaddr

        if((fp_sock_r = fdopen(clntsock, "rb")) == NULL){
            die("fdopen read failed");
        }
        if ((fp_sock_w = fdopen(clntsock, "wb")) == NULL){
            die("fdopen write failed");
        }
        */

        string req_line = clntsock.read_ln(buf);

        string[] tokens = new string[3];
        req_line.split(" ", tokens, 3);

        string method = tokens[0];
        string req_URI = tokens[1];
        string httpVersion = tokens[2];

        string fileName;

        string respHeader;
        int size;

        if (method.length() == 0 || httpVersion.length() == 0 ){
            respHeader = "501 Not Implemented";
        }
        else if (method != "GET" || (httpVersion != "HTTP/1.0" && httpVersion != "HTTP/1.1")
            respHeader = "501 Not Implemented"

        else if (req_URI[0] != '/' || strstr(request_URI, "/../") != NULL || strncmp(request_URI+(strlen(request_URI)-3),"/..", 3) == 0){
            sprintf(respHeader, "400 Bad Request");
        }

        else
        {
            fileName += webroot;
            fileName += request_URI;

            // if uri ends with a '/', append index.html
            if (req_URI[req_URI.length() - 1] == '/'){
                fileName += "index.html";
            }
            // Check if the URI is a directory
            // stat(fileName, &stat_buffer);
            // if (stat(fileName, &stat_buffer) == -1 ){
            //     sprintf(respHeader, "404 Not Found");
            //     }
            // else if (S_ISDIR(stat_buffer.st_mode) == 1){
            //     // concatenate /index.html on request_URI
            //     strcat(fileName, "/index.html");
            // }

            file target;
            // Try to open the file or give a 404
            if ((target = fopen(fileName, "rb")) == NULL){
                respHeader = "404 Not Found";
            }
            else{
                respHeader = "200 OK";
            }
        }

        char temp_buffer[100];
        char neutral_buf[1000];

        if(strcmp(respHeader, "200 OK") == 0 || strcmp(respHeader, "404 Not Found") == 0){
            //fprintf(stderr,"inside the 200 Ok or the 404 NOT found... need one more line");

            while(strcmp(fgets(neutral_buf, sizeof(neutral_buf), fp_sock_r), "\r\n")!=0 && strcmp(neutral_buf,"\n")!=0);
        }

        size = sprintf(temp_buffer, "HTTP/1.0 %s \r\n\r\n", respHeader);
        if (fwrite(temp_buffer,1,size,fp_sock_w) != size){
            die("sending response header failed");
        }

        char looking_up[500] = "";


        if (strcmp(respHeader, "200 OK") == 0){

        // If the request is for a file and not for /mdb-lookup or /mdb-lookup?key=
            // Format and print the correct logging output similar to Jae's
            fprintf(stderr, "%s%s \"%s %s %s\" %s\n",looking_up,inet_ntoa(clntaddr.sin_addr),
                    (request_method ? request_method : "(null)"),
                    (request_URI ? request_URI : "(null)"),
                    (httpVersion ? httpVersion : "(null)"),respHeader);

            // Send the file

            while((size = fread(buf, 1, sizeof(buf), fp_target)) > 0){
                if (fwrite(buf, 1, size, fp_sock_w) != size){
                    die("sending fwrite failed");
                }
            }
            fflush(fp_sock_w);
            fclose(fp_target);
        }

        else{
            // If the request returned a 404, 400 or 501

            // Format and print the correct logging output similar to Jae's
            fprintf(stderr, "%s%s \"%s %s %s\" %s\n",looking_up,inet_ntoa(clntaddr.sin_addr),
                  (request_method ? request_method : "(null)"),
                  (request_URI ? request_URI : "(null)"),
                  (httpVersion ? httpVersion : "(null)"),respHeader);


            size = snprintf(buf,sizeof(buf),
                    "<html><body><h1>%s</h1></body></html>", respHeader);
            if (size != fwrite(buf, 1, size, fp_sock_w)){
                die("write response failed");
            }
            fflush(fp_sock_w);
        }


        // Finally, close the client connection and go back to accept()
        fclose(fp_sock_w);
        fclose(fp_sock_r);

        close(clntsock);

    }

}