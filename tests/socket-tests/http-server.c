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
    while (1) {

        // Accept an incoming connection

	    socket clntsock = naccept(listener);
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


        else
        {
            fileName += webroot;
            fileName += request_URI;

            // if uri ends with a '/', append index.html
            if (req_URI[req_URI.length() - 1] == '/'){
                fileName += "index.html";
            }

            file targetFile;
            // Try to open the file or give a 404
            if ((targetFile = fopen(fileName, "rb")) == L){
                respHeader = "404 Not Found";
            }
            else{
                respHeader = "200 OK";
            }
        }
        // log the request
        stdout.writeln(request_method + " " + req_URI + " " + httpVersion + " " + respHeader);
        char temp_buffer[100];
        char neutral_buf[1000];

        if (respHeader == "200 OK" || respHeader == "404 Not Found") {
            // read socket until end of header/body of request
            //fprintf(stderr,"inside the 200 Ok or the 404 NOT found... need one more line");

            while(strcmp(fgets(neutral_buf, sizeof(neutral_buf), fp_sock_r), "\r\n")!=0 && strcmp(neutral_buf,"\n")!=0);
        }

        clntsock.writeln("HTTP/1.0 " + respHeader + "\r\n\r\n");

        if(respHeader == "200 OK"){
            // TODO
            // Send the file
            // source file: targetFile
            // dest socket: clntsock
            targetFile.close();
        }
        else{
            // If the request returned a 404, 400 or 501
            string htmlResponse = "<html><body><h1>" + respHeader + "</h1></body></html>";
            clntsock.write(htmlResponse);
        }

        clntsock.close();
    }

}