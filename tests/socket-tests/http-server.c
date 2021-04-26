
int main(string[] argv)
{
	/* language provided array has built in length and safe member access */
	if (argv.alength() != 3){
		/* stdout/stdin is just another file */
		stdout.writeln("usage: " + argv[0] + " <server-port> <web-root>");
		return -1;
	}

	string port = argv[1];
	string webroot = argv[2];

	/* simple socket creation/deletion */
	// Create a listening socket (also called server socket)
	socket listener = nopen("", port.toint(), "tcp", "listen");

	socket clntsock = listener.naccept(); /* gives connected socket */
	/* files/sockets have similar well-defined read/write interfaces */
	string req_line = clntsock.readln();
	stdout.writeln(req_line);

	// Accept an incoming connection
	string[] tokens = new string[3]{"", "", ""};
	req_line.split(" ", tokens); /* standard library operations on strings are very convenient */

	/* never have to worry about string management, everything is automatically cleaned up */
	string method = tokens[0];
	string req_URI = tokens[1];
	string httpVersion = tokens[2];

	stdout.writeln(method + " " + req_URI + " " + httpVersion);

	string fileName;
	string respHeader;
	int size;

	if (method.slength() == 0 || httpVersion.slength() == 0 ){
		respHeader = "501 Not Implemented";
	}

	else if (method != "GET" || (httpVersion != "HTTP/1.0" && httpVersion != "HTTP/1.1")) {
		respHeader = "501 Not Implemented";
	}
	stdout.writeln(respHeader);
	/*
	   else
	   {
	   fileName += webroot;
	   fileName += req_URI;

	// if uri ends with a '/', append index.html
	if (req_URI[req_URI.slength() - 1] == '/'){
	fileName += "index.html";
	}

	file targetFile;
	// Try to open the file or give a 404
	targetFile = fopen(fileName, "rb");
	if (targetFile.error()){
	respHeader = "404 Not Found";
	}
	else{
	respHeader = "200 OK";
	}
	}

	// log the request
	stdout.writeln(req_URI + " " + httpVersion + " " + respHeader);


	/*
	while (1) {



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
	*/

	return 0;

}
