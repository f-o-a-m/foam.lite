#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>  /* directory listing */
#include <string.h>  /* c strings */
#include <fcntl.h>   /* File Control Definitions           */
#include <termios.h> /* POSIX Terminal Control Definitions */
#include <unistd.h>  /* UNIX Standard Definitions          */
#include <errno.h>   /* ERROR Number Definitions           */

const int SERIAL_OPENING_FLAGS = O_RDWR | O_NOCTTY;
/* O_RDWR   - Read/Write access to serial port       */
/* O_NOCTTY - No terminal will control the process   */
/* Open in blocking mode,read will wait              */


int find_serial(char* destination) {
  DIR *d;
  struct dirent *dir;
  const char* base_dir = "/dev/serial/by-id";
  d = opendir(base_dir);
  if (d) {
    while ((dir = readdir(d)) != NULL) {
      if (strcmp(dir->d_name, ".") == 0 || strcmp(dir->d_name, "..") == 0) {
          continue; // skip . and ..
      }
      if (strstr(dir->d_name, "STM32") == NULL) {
        sprintf(destination, "%s/%s", base_dir, dir->d_name);
        printf("found candidate %s\n", destination);
        closedir(d);
        return 0;
      }
    }
    closedir(d);
  }
  printf("couldnt find a serial port that doesnt have STM32 in the name :(\n");
  return -1;
}

void main(void)
{
        char* serial_path = calloc(2048, sizeof(char));
        if (find_serial(serial_path) != 0) {
            printf("couldn't autodetect the serial path, expect trouble\n");
        }
        int fd = open(serial_path, SERIAL_OPENING_FLAGS);/*File Descriptor*/

        printf("\n +----------------------------------+");
        printf("\n |        Serial Port Read          |");
        printf("\n +----------------------------------+");
        /*------------------------------- Opening the Serial Port -------------------------------*/
        if(fd == -1)                                    /* Error Checking */
           printf("\n  Error! in Opening %s  ", serial_path);
        else
           printf("\n  %s Opened Successfully ", serial_path);


/*---------- Setting the Attributes of the serial port using termios structure --------- */
		
	struct termios SerialPortSettings;	/* Create the structure                          */

	tcgetattr(fd, &SerialPortSettings);	/* Get the current attributes of the Serial port */

	/* Setting the Baud rate */
	cfsetispeed(&SerialPortSettings,B115200); /* Set Read  Speed as 115200                       */
	cfsetospeed(&SerialPortSettings,B115200); /* Set Write Speed as 115200                       */

	/* 8N1 Mode */
	SerialPortSettings.c_cflag &= ~PARENB;   /* Disables the Parity Enable bit(PARENB),So No Parity   */
	SerialPortSettings.c_cflag &= ~CSTOPB;   /* CSTOPB = 2 Stop bits,here it is cleared so 1 Stop bit */
	SerialPortSettings.c_cflag &= ~CSIZE;	 /* Clears the mask for setting the data size             */
	SerialPortSettings.c_cflag |=  CS8;      /* Set the data bits = 8                                 */
		
	SerialPortSettings.c_cflag &= ~CRTSCTS;       /* No Hardware flow Control                         */
	SerialPortSettings.c_cflag |= CREAD | CLOCAL; /* Enable receiver,Ignore Modem Control lines       */ 
		
	SerialPortSettings.c_iflag &= ~(IXON | IXOFF | IXANY);          /* Disable XON/XOFF flow control both i/p and o/p */
	SerialPortSettings.c_iflag &= ~(ICANON | ECHO | ECHOE | ISIG);  /* Non Cannonical mode                            */

	SerialPortSettings.c_oflag &= ~OPOST;/*No Output Processing*/
		
	/* Setting Time outs */
	SerialPortSettings.c_cc[VMIN] = 10; /* Read at least 10 characters */
	SerialPortSettings.c_cc[VTIME] = 0; /* Wait indefinetly   */


	if((tcsetattr(fd,TCSANOW,&SerialPortSettings)) != 0) /* Set the attributes to the termios structure*/
	    printf("\n  ERROR ! in Setting attributes");
	else
            printf("\n  BaudRate = 115200 \n  StopBits = 1 \n  Parity   = none");
			
        /*------------------------------- Read data from serial port -----------------------------*/

	tcflush(fd, TCIFLUSH);   /* Discards old data in the rx buffer            */

	char read_buffer[32];   /* Buffer to store the data received              */
	int  bytes_read = 0;    /* Number of bytes read by the read() system call */
	int i = 0;

	bytes_read = read(fd,&read_buffer,32); /* Read the data                   */
			
	printf("\n\n  Bytes Rxed -%d", bytes_read); /* Print the number of bytes read */
	printf("\n\n  ");

	for(i=0;i<bytes_read;i++)	 /*printing only the received characters*/
	    printf("%c",read_buffer[i]);
	
	printf("\n +----------------------------------+\n\n\n");

	close(fd); /* Close the serial port */

}
