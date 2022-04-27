000100 IDENTIFICATION DIVISION. 
000200 PROGRAM-ID "Customer Data". 
000300 ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT CustomerFile ASSIGN TO "customer.txt"

               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS iDNUM.


000400 DATA DIVISION.
       FILE SECTION. 
       FD CustomerFile.
       01 CustomerData.
           02 iDNUM PIC 99.
           02 FIRSTNAME PIC X(15).
           02 LASTNAME PIC X(15).

       WORKING-STORAGE SECTION.
       01 Choice PIC 9.
       01 StayOpen PIC X VALUE "Y".
       01 CostumerExists PIC X.

       PROCEDURE DIVISION.
       StartPara.
           OPEN I-O CustomerFile.
           PERFORM UNTIL StayOpen= "N"
               DISPLAY ""
               DISPLAY "CUSTOMER RECORDS"
               DISPLAY "1: Add Customer"
               DISPLAY "2: Delete Customer"
               DISPLAY "3: Update Customer"
               DISPLAY "4: Get Customer"
               DISPLAY "0: Quit"
               DISPLAY ": " WITH NO ADVANCING
               ACCEPT Choice
               EVALUATE Choice
                  WHEN 1 PERFORM AddCust
                  WHEN 2 PERFORM DeleteCust
                  WHEN 3 PERFORM UpdateCust
                  WHEN 4 PERFORM GetCust
                  WHEN OTHER MOVE "N" TO StayOpen
                END-EVALUATE
               END-PERFORM.
           CLOSE CustomerFile 
           STOP RUN.

       AddCust.
              DISPLAY " "
              DISPLAY "Enter ID : " WITH NO ADVANCING .
              ACCEPT  iDNUM.
              DISPLAY "Enter First Name : " WITH NO ADVANCING .
              ACCEPT FIRSTNAME .
              DISPLAY "Enter Last Name : " WITH NO ADVANCING 
              ACCEPT LASTNAME .
              DISPLAY  "  "
              WRITE CustomerData 
                   INVALID KEY DISPLAY "ID Taken"

       DeleteCust.
                 DISPLAY ""
                 DISPLAY "Enter Customer ID to delete: " WITH NO ADVANCING.
                 ACCEPT iDNUM.
                 DELETE CustomerFile
                     INVAlid KEY DISPLAY  "Key Doesnt Exist"
                 END-DELETE.

       Updatecust.
                 Move "Y" TO CostumerExists.
                 Display ""
                 DISPLAY "Enter ID To Update :" WITH NO ADVANCING.
                 ACCEPT iDNUM.
                 READ CustomerFile
                     INVALID KEY MOVE "N" TO CostumerExists 
                 END-READ
                 IF CostumerExists = "N"
                     DISPLAY "Customer Dpesnt Exist"
                 ELSE 
                    DISPLAY  "Enter new first name: " WITH NO ADVANCING .
                    ACCEPT FIRSTNAME
                    DISPLAY  "Enter new last name: " WITH NO ADVANCING .
                    ACCEPT LASTNAME 
                 END-IF.
                 REWRITE CustomerData   
                     INVALID KEY DISPLAY "Customer not updated"
                 END-REWRITE. 
       GetCust.
              MOVE "Y" TO CostumerExists.
              DISPLAY ""
              DISPLAY "ENter Customer Id To Find :" WITH NO ADVANCING .
              ACCEPT iDNUM .
              READ CustomerFile 
                  INVALID KEY MOVE "N" To CostumerExists.
              END-READ

              IF CostumerExists  = "N"
                 DISPLAY "Costumer Doesn't exist"
              ELSE
                DISPLAY "ID : "iDNUM 
                DiSPLAY "First name: " FIRSTNAME 
                DISPLAY "Last name: " LASTNAME  
              END-IF. 

            
 

                     
                             


            

                