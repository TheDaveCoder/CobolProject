       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "ORDERINGSYSTEM".
       AUTHOR.     GROUP-5.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT PRINT-FILE ASSIGN TO "RECEIPT-OUTPUT.DAT".

       DATA DIVISION.
       FILE SECTION.
         FD PRINT-FILE.
         01 PRINT-LINE         PIC X(132).

       WORKING-STORAGE SECTION.
       01 ORDER-VALUES.
           02 CONT-ORDER           PIC X VALUE "Y".
           02 LOOP-COUNTER         PIC 9 VALUE 1.
           02 ORDER-NUM            PIC 9 VALUE 1.
           02 ORDER-LOAD           PIC 9.
           02 ORDER-PRICES OCCURS 3 TIMES.
             03 ORDER-PRICE        PIC 9(3)V9(2).
           02 ORDER-CODES OCCURS 3 TIMES.
             03 ORDER-CHOICE       PIC X(2).
           02 ORDER-QTY OCCURS 3 TIMES.
             03 ORDER-PCS          PIC 9(2).
           02 ORDER-TOTAL          PIC 9(3)V9(2).

       01 INFO-LINE.
           02 FILLER           PIC X(15) VALUE "CUSTOMER NAME".
           02 FILLER           PIC X(2) VALUE SPACES.
           02 FILLER           PIC X(15) VALUE "ADDRESS".
           02 FILLER           PIC X(2) VALUE SPACES.
           02 FILLER           PIC X(9) VALUE "PHONE NUM".

       01 DETAIL-INFO-LINE.
           02 DET-NAME         PIC X(15).
           02 FILLER           PIC X(2) VALUE SPACES.
           02 DET-ADDRESS      PIC X(15).
           02 FILLER           PIC X(2) VALUE SPACES.
           02 DET-CNUM         PIC X(9).
       
       01 WS-CURRENT-DATE-DATA.
           02 WS-CURRENT-DATE.
             03 WS-CURRENT-YEAR PIC 9(4).
             03 WS-CURRENT-MONTH PIC 9(2).
             03 WS-CURRENT-DAY PIC 9(2).
           02 WS-CURRENT-TIME.
             03 WS-CURRENT-HOURS PIC 9(2).
             03 WS-CURRENT-MINUTE PIC 9(2).
             03 WS-CURRENT-SECOND PIC 9(2).
             03 WS-CURRENT-MILLISECONDS PIC 9(2).

       01 FIXED-DT.
           02 FORMATTED-DT.
             03 WS-F-YEAR PIC 9(4).
             03 WS-FILLER PIC X VALUE '-'.
             03 WS-F-MONTH PIC 9(2).
             03 WS-FILLER PIC X VALUE '-'.
             03 WS-F-DAY PIC 9(2).
             03 WS-FILLER PIC X VALUE SPACE.
             03 WS-F-HOUR PIC 9(2).
             03 WS-FILLER PIC X VALUE ':'.
             03 WS-F-MIN PIC 9(2).

       01 ORDER-LINE.
           02 FILLER           PIC X(5) VALUE "ORDER".
           02 FILLER           PIC X(2) VALUE SPACES.
           02 FILLER           PIC X(8) VALUE "QUANTITY".
           02 FILLER           PIC X(2) VALUE SPACES.
           02 FILLER           PIC X(8) VALUE "PRICE".

       01 DETAIL-ORDER-LINE.
           02 DET-ORDER        PIC X(5).
           02 FILLER           PIC X(2) VALUE SPACES.
           02 DET-PCS          PIC X(8).
           02 FILLER           PIC X(2) VALUE SPACES.
           02 DET-PRICE        PIC 999.99.

       01 HORIZONTAL-RULE      PIC X(25) VALUE 
           "-------------------------".
       01 TOTAL-LINE.
           02 FILLER           PIC X(5) VALUE "TOTAL".
           02 FILLER           PIC X(7) VALUE SPACES.
           02 FILLER           PIC X(4) VALUE "PHP ".
           02 DET-TOTAL        PIC 999.99 VALUE 000.00.

            
       PROCEDURE DIVISION.
           PERFORM 0050-START-PROGRAM.

       
       0050-START-PROGRAM.
           OPEN OUTPUT PRINT-FILE.
           PERFORM 0100-ORDER-LOOP.
           PERFORM 0200-LOG-CREDENTIALS.
           PERFORM 0300-PROCESS-RECEIPT.
       
       0100-ORDER-LOOP.

           PERFORM VARYING LOOP-COUNTER FROM 1 BY 1 UNTIL 
           LOOP-COUNTER > 3
             PERFORM 0150-MENU
             COMPUTE ORDER-LOAD = ORDER-LOAD + 1
             DISPLAY "ORDER CODE >> "
             ACCEPT ORDER-CHOICE(LOOP-COUNTER)
             MOVE FUNCTION UPPER-CASE(ORDER-CHOICE(LOOP-COUNTER)) TO
             ORDER-CHOICE(LOOP-COUNTER)
             EVALUATE ORDER-CHOICE(LOOP-COUNTER)
               WHEN "C1"
                 MOVE 120 TO ORDER-PRICE(LOOP-COUNTER)
               WHEN "C2"
                 MOVE 180.50 TO ORDER-PRICE(LOOP-COUNTER)
               WHEN "C3"
                 MOVE 210.90 TO ORDER-PRICE(LOOP-COUNTER)
               WHEN "P1"
                 MOVE 160.25 TO ORDER-PRICE(LOOP-COUNTER)
               WHEN "P2"
                 MOVE 200 TO ORDER-PRICE(LOOP-COUNTER)
               WHEN "P3"
                 MOVE 370.95 TO ORDER-PRICE(LOOP-COUNTER)
               WHEN OTHER
                 DISPLAY "ERROR ORDER CHOICE, NOT IN THE MENU"
                 STOP RUN
             END-EVALUATE
             DISPLAY " "
             DISPLAY "ORDER QTY >> "
             ACCEPT ORDER-PCS(LOOP-COUNTER)
             DISPLAY " "
             IF LOOP-COUNTER NOT EQUAL 3
               DISPLAY "ORDER ANOTHER ITEM? Y/N >> "
               ACCEPT CONT-ORDER
               MOVE FUNCTION UPPER-CASE(CONT-ORDER) TO CONT-ORDER
               IF CONT-ORDER = "N"
                 MOVE 4 TO LOOP-COUNTER
               ELSE
                 DISPLAY "-----INCORRECT INPUT CODE, WILL PROCEED TO CHECKOUT-----"
                 MOVE 4 TO LOOP-COUNTER
               END-IF
             END-IF
           END-PERFORM.   

       0150-MENU.
           DISPLAY "█████████╗ ██╗   ██╗ █████████╗  █████████╗ █████████╗ ███████╗ █████████╗ ██╗ █████████╗".
           DISPLAY "███   ███║ ██║   ██║ ███   ███║   ╚═════██║  ╚═════██║ ██╔════╝ ███   ███║ ██║ ███   ███║".
           DISPLAY "█████████║ ██║   ██║ █████████║     ╔███╔═╝    ╔███╔═╝ █████╗   █████████║ ██║ █████████║".
           DISPLAY "██╔══════╝ ██║   ██║ ██╔══════╝   ╔███╔═╝    ╔███╔═╝   ██╔══╝   ██████═══╝ ██║ ██╔═══╗██║".
           DISPLAY "██║        ╚██████╔╝ ██║         █████████╗ █████████╗ ███████╗ ██║╚═╗███╗ ██║ ██║   ║██║".
           DISPLAY "╚═╝         ╚═════╝  ╚═╝         ╚════════╝ ╚════════╝ ╚══════╝ ╚═╝  ╚═══╝ ╚═╝ ╚═╝   ╚══╝".
           DISPLAY " ".
           DISPLAY " ".
           DISPLAY "███╗   ███╗███████╗███╗  ██╗██╗   ██╗██╗".
           DISPLAY "████╗ ████║██╔════╝████╗ ██║██║   ██║╚═╝".
           DISPLAY "██╔████╔██║█████╗  ██╔██╗██║██║   ██║   ".
           DISPLAY "██║╚██╔╝██║██╔══╝  ██║╚████║██║   ██║   ".
           DISPLAY "██║ ╚═╝ ██║███████╗██║ ╚███║╚██████╔╝██╗".
           DISPLAY "╚═╝     ╚═╝╚══════╝╚═╝  ╚══╝ ╚═════╝ ╚═╝".
           DISPLAY " ".
           DISPLAY "█▀▀ █   ▄▀█ █▀ █▀ █ █▀▀ ▀".
           DISPLAY "█▄▄ █▄▄ █▀█ ▄█ ▄█ █ █▄▄ ▄".
           DISPLAY "C1 - ₱120.00 CLASSIC PAN PIZZA".
           DISPLAY "C2 - ₱180.50 HAWAIIAN GALORE PIZZA".
           DISPLAY "C3 - ₱210.90 ULTRA MEATY LOVERS PIZZA".
           DISPLAY " ".
           DISPLAY "█▀█ ▄▀█ █▀ ▀█▀ ▄▀█ ▀".
           DISPLAY "█▀▀ █▀█ ▄█  █  █▀█ ▄".
           DISPLAY "P1 - ₱160.25 1PC CHICKEN, SPAGHETTI, 1 SLICE CLASSIC PIZZA, REGULAR DRINK".
           DISPLAY "P2 - ₱200.00 1PC CHICKEN, SPAGHETTI, 1 SLICE HAWAIIAN PIZZA REGULAR FRIES, MEDIUM DRINK".
           DISPLAY "P3 - ₱370.95 6PCS CHICKEN NUGGETS, SPAGHETTI, 1 SLICE ULTRA MEATY PIZZA, CHEESE BURGER, LARGE DRINK".
           DISPLAY " ".
           DISPLAY "(3 ORDERS MAX)".
           DISPLAY " ".

       0200-LOG-CREDENTIALS.
           DISPLAY " ".
           DISPLAY "-- CUSTOMER DETAILS --"
           DISPLAY " ".
           DISPLAY "CUSTOMER NAME: ".
           ACCEPT DET-NAME.
           DISPLAY " ".
           DISPLAY "ADDRESS: ".
           ACCEPT DET-ADDRESS.
           DISPLAY " ".
           DISPLAY "PHONE NUMBER: "
           ACCEPT DET-CNUM.

       0300-PROCESS-RECEIPT.
           PERFORM 0310-PRINT-INFO-LINE.
           PERFORM 0320-PRINT-ORDER-LINE.
           PERFORM 0330-CALCULATE-ORDER.
           PERFORM 0340-PRINT-TOTAL-LINE.

       0310-PRINT-INFO-LINE.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA.
           MOVE WS-CURRENT-YEAR TO WS-F-YEAR.
           MOVE WS-CURRENT-MONTH TO WS-F-MONTH.
           MOVE WS-CURRENT-DAY TO WS-F-DAY.
           MOVE WS-CURRENT-HOURS TO WS-F-HOUR.
           MOVE WS-CURRENT-MINUTE TO WS-F-MIN.
           WRITE PRINT-LINE FROM FORMATTED-DT.
           WRITE PRINT-LINE FROM INFO-LINE.
           WRITE PRINT-LINE FROM DETAIL-INFO-LINE 
             AFTER ADVANCING 1 LINE.
       
       0320-PRINT-ORDER-LINE.
           WRITE PRINT-LINE FROM ORDER-LINE AFTER ADVANCING 4 LINE.
           MOVE 0 TO ORDER-NUM.
           PERFORM VARYING ORDER-NUM FROM 1 BY 1 UNTIL 
           ORDER-NUM > ORDER-LOAD
             MOVE ORDER-CHOICE(ORDER-NUM) TO DET-ORDER
             MOVE ORDER-PCS(ORDER-NUM) TO DET-PCS
             MOVE ORDER-PRICE(ORDER-NUM) TO DET-PRICE
             WRITE PRINT-LINE FROM DETAIL-ORDER-LINE
               AFTER ADVANCING 1 LINE
           END-PERFORM.
           
       0330-CALCULATE-ORDER.
           MOVE 0 TO ORDER-NUM.
           PERFORM VARYING ORDER-NUM FROM 1 BY 1 UNTIL 
           ORDER-NUM > ORDER-LOAD
             MULTIPLY ORDER-PRICE(ORDER-NUM) BY 
             ORDER-PCS(ORDER-NUM) GIVING ORDER-PRICE(ORDER-NUM)
             COMPUTE ORDER-TOTAL = ORDER-PRICE(ORDER-NUM) + ORDER-TOTAL
           END-PERFORM.

       0340-PRINT-TOTAL-LINE.
           WRITE PRINT-LINE FROM HORIZONTAL-RULE AFTER ADVANCING 1 LINE.
           MOVE ORDER-TOTAL TO DET-TOTAL.
           WRITE PRINT-LINE FROM TOTAL-LINE AFTER ADVANCING 1 LINE.

           CLOSE PRINT-FILE.
           STOP RUN.
           END PROGRAM ORDERINGSYSTEM.

