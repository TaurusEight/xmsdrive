 linesize 132           ; Compiler command to use listing width of 132 charators
;****************************************************************************************
;
;       Extended Memory RAM disk
;
;       Written by
;
;       Daniel F. Mendyke in September 1992
;
;       Copyright (c) 1992 by Daniel F. Mendyke, all rights reserved
;
;       Usage:
;

;****************************************************************************************
;   Data Section
;****************************************************************************************
            org 0                 ; Divice Driver offset
            ;============================================================================
devicechain dw  -1                ; Space for adress of next driver
            dw  -1
            ;============================================================================
attribute   dw  0h                ; Block device driver
            ;============================================================================
            dw  offset strategy   ; Offset into strategy routine
            dw  offset command    ; Offset into command routine
            ;============================================================================
numofdrv    db 01h                ; Number of devices this driver supports
            ;============================================================================
reqoffset   dw  0                 ; Offset of the request header
reqsegment  dw  0                 ; Segment of the request header
            ;============================================================================
bpbptr      dw offset bytespersec ; Pointer to the BIOS parmeter block
            ;============================================================================
bytespersec dw 256     ; Default number of bytes per sector
secperclust db 1       ; Default number of sector per cluster, allways one
ressectors  dw 1       ; Number of reserved sectors, again allways one
fats        db 1       ; Number of FATs on disk, again allways one
rootdirents dw 64      ; Default number of root directory entires
sectors     dw 1024    ; Default number of sectors on the disk
media       db 0f8h    ; Media discriptor byte
fatsecs     dw 6       ; Default number of sectors used by FAT table
secpertrack dw 1       ; Number of sectors per track
heads       dw 1       ; Number of heads per disk
hiddensecs  dw 0       ; Number of hidden sectors
            dw 0
hugesectors dw 0       ; Number of hudge sectors if the disk is greater than 32MB
            dw 0
            ;============================================================================
embhandle   dw 0       ; Handle to XMS memory used by RAM disk
            ;============================================================================
xmsadress label dword  ; Pointer to the XMS driver in memory
xmsoffset   dw 0       ; Offset of the XMS driver pointer
xmssegment  dw 0       ; Segment of the XMS driver pointer

;****************************************************************************************
;   EMM structure
;****************************************************************************************
lowbytes    dw 0       ; Low part of number of bytes to move
highbytes   dw 0       ; High part of number of bytes to move
sourcehand  dw 0       ; Source handle, if zero then in lower memory
sourceoff   dw 0       ; Offset into the source block, or offset of location
sourceseg   dw 0       ; Segment of location or high bytes of source offset if in XMS
destinhand  dw 0       ; Destination handle, if zero than in lower memory
destinoff   dw 0       ; Offset into destination memory block
destinseg   dw 0       ; Segment of destination block or high bytes if in XMS

;****************************************************************************************
;  Index of function routine locations
;****************************************************************************************
index       dw  offset  initalize       ; #0
            dw  offset  media_check     ; #1
            dw  offset  build_bpb       ; #2
            dw  offset  read_data       ; #3
            dw  offset  read_data       ; #4
            dw  offset  bad_function    ; #5
            dw  offset  bad_function    ; #6
            dw  offset  bad_function    ; #7
            dw  offset  write_data      ; #8
            dw  offset  write_data      ; #9

;****************************************************************************************
;   Strategy Routine
;****************************************************************************************
strategy    proc far                ; Save adress of DOS request header
            seg cs                  ; Use CS register as default
            mov reqoffset, bx       ; Save offset of request header
            seg cs                  ; Use CS register as default
            mov reqsegment, es      ; Save segment of request header
            ret                     ; Return to DOS
            endp

;****************************************************************************************
;   Command Routine
;****************************************************************************************
command     proc far            ; Perform commmand function supplied by DOS
            pushf               ; Save flag register
            pusha               ; Save all general use registers
            push es             ; Save ES and DS registers
            push ds
            ;============================================================================
            seg cs              ; Use code segment rather than default
            les di, reqoffset   ; Get offset and segment of request header
            xor bx, bx          ; Clear BX
            seg es              ; Use ES rather than the default DS
            mov bl, [di+2]      ; Get function number
            ;============================================================================
            cmp bl, 09h         ; Check function to see if it is supported
            jbe goodfunct       ; Function is supported so conitinue
            mov bl, 5           ; Funciton is not supported so use error function
goodfunct   shl bl, 1           ; Multiply by two
            seg cs              ; Use code segment rather than default DS
            call word [bx + offset index ]   ; Call correct routine
            jc  loadstatus      ; Check if there was an error
            mov ax, 0100h       ; Set all done bit in ax
loadstatus  mov [di+3], ax      ; Load value into request header
            ;============================================================================
            pop ds              ; Restore DS and ES registers
            pop es
            popa                ; Restore all general use registers
            popf                ; Restore flag register
            retf                ; End command
            endp                ; End of device driver

;****************************************************************************************
;   Bad Function Number Routine #3, #5, #6, #7
;****************************************************************************************
bad_function proc near      ; Unsupported command routine
            mov ax, 8103h   ; Error code for bad function
            stc             ; Set carry flag to show an error
            retn            ; End bad funtion number
            endp

;****************************************************************************************
;   Media Check Routine  #1
;****************************************************************************************
media_check proc near               ; Tell DOS that the disk has not been changed!
            seg es                  ; Use passed segment!
            mov byte [di+14], 01h   ; Media has not changed
            clc                     ; Clear the carry flag to show no error
            retn                    ; End media check
            endp

;****************************************************************************************
;   Build Bios Perameter Block Routine  #2
;****************************************************************************************
build_bpb   proc near                ; Give DOS adress of BIOS parameter block
            seg es                   ; Use ES rather than default
            mov word [di+18], offset bytespersec ; Load offset of BPB
            seg es                   ; Use ES rather than default DS
            mov word [di+20], cs     ; Move segment into request header
            clc                      ; Clear carry to show no errors
            retn                     ; End build bpb
            endp

;****************************************************************************************
;   Read data to disk sector - DS:SI points to DOS request header
;****************************************************************************************
read_data   proc near               ; Read data to disk sector
            call setblock           ; Call routine to setup EMB block with correct values
            pop sourcehand          ; Pop the sourcehandle off of the stack
            pop destinseg           ; Move into EMM struct the segment where the data is
            pop destinoff           ; Move into EMM struct the offset where the data is
            ;============================================================================
            mov sourceoff, ax       ; Store low bytes of offset into XMS block
            mov sourceseg, dx       ; Store high bytes into offset of XMS block
            ;============================================================================
            mov word destinhand, bx ; Use zero as source handle
            ;============================================================================
            mov ah, 0bh             ; Load AH with function number to move XMS memory
            call far xmsadress      ; Call XMS driver to move data into disk
            ;============================================================================
            dec ax                  ; Was function suggessfull?
            jz readsuccss           ; Yes
            ;============================================================================
            mov ax, 810bh           ; Give DOS a write fault error message
            stc                     ; Set carry to show error
            retn                    ; Return to calling program
            ;============================================================================
readsuccss  clc                     ; Clear carry to show all is well
            retn                    ; Return to calling program
            endp

;****************************************************************************************
;   Write data to disk sector - DS:SI points to DOS request header
;****************************************************************************************
write_data  proc near               ; Writes data to disk sector
            call setblock           ; Call the routine that setup the EMB block
            pop destinhand          ; Pop the destination handle from the stack
            pop sourceseg           ; Move into EMM struct the segment where the data is
            pop sourceoff           ; Move into EMM struct the offset where the data is
            ;============================================================================
            mov destinoff, ax       ; Store low bytes of offset into XMS block
            mov destinseg, dx       ; Store high bytes into offset of XMS block
            ;============================================================================
            mov word sourcehand, bx ; Use zero as source handle
            ;============================================================================
            mov ah, 0bh             ; Load AH with function number to move XMS memory
            call far xmsadress      ; Call XMS driver to move data into disk
            ;============================================================================
            dec ax                  ; Was function suggessfull?
            jz writesuccss          ; Yes
            ;============================================================================
            mov ax, 810ah           ; Give DOS a write fault error message
            stc                     ; Set carry to show error
            ret                     ; Return to calling program
            ;============================================================================
writesuccss clc                     ; Clear carry to show all is well
            ret                     ; Return to calling program
            endp

;****************************************************************************************
;   Set up the EMB block and get all data from DOS request header
;****************************************************************************************
setblock    proc near              ; Setup the EMB block used by the XMS driver
            push cs                ; Push the code segment driver
            pop ds                 ; Pop the default data regester to point to CS
            ;============================================================================
            xor dx, dx             ; Zero out DX so it doesn't interfere
            seg es                 ; Use ES rather than the default DS
            mov ax, word [di+18]   ; Get number of sectors to move
            mov bx, ax             ; Save number in CX
            mul bytespersec        ; Get total number of bytes to be moved
            mov lowbytes, ax       ; Save number of bytes to be moved
            mov highbytes, dx      ; Save high order part of bytes to be moved
            ;============================================================================
            seg es                 ; Use ES rather than default DS
            mov ax, word [di+20]   ; Get starting disk sector where move is to take place
            add bx, ax             ; Add number of sectors to move and starting sector
            jc notfound            ; Carry means that the sector is off the disk
            cmp bx, sectors        ; Compare to the total number of sectors on the disk
            ja notfound            ; Sector is not on the disk so an error has occured
            ;============================================================================
            xor dx, dx             ; Zero out DX
            mul bytespersec        ; Find the offset of the starting sector on the disk
            pop bx                 ; Get return adress of calling function
            seg es                 ; Use ES rather than default DS
            push word [di+14]      ; Push offset of DOS transfere buffer
            seg es                 ; Use ES rather than default DS
            push word [di+16]      ; Push segment of DOS transfere buffer
            push embhandle         ; Store EMB handle used by disk on the stack
            push bx                ; Return adress of calling routine to stack
            xor bx, bx             ; Zero out BX
            mov si, offset lowbytes ; Load SI with EMM struct offset, DS:SI points to EMM
            retn                   ; Return to calling program
            ;============================================================================
notfound    pop ax                 ; Pop adress of calling routine, leave adress of main
            seg es                 ; Use ES rather than default DS
            mov word [di+18], 0    ; Tell DOS that no sectors were moved
            mov ax, 8108h          ; Error code for secoter not found
            stc                    ; Set carry to show error
            retn                   ; return to main routine not calling routine
            endp

;****************************************************************************************
;   Setup the xms drive and return all values needed by dos
;****************************************************************************************
initalize   proc near              ; Initialize the disk driver
            call printopening      ; Print opening header
            call readuservalues    ; Read any values from the command line tail
            call getxmsblock       ; Try to reserve a block of xms memory
            call movebpbtosec      ; Move the BIOS parm block to the disk boot sector
            call movesectors       ; Move all sectors to the disk
            call givetodos         ; Give DOS requested info about the drive
            call printinfo         ; Print size of disk and sector and ents in root
            clc                    ; Set no error flag so calling routine exits correctly
            ret                    ; Return to main routine
            endp

;****************************************************************************************
;   Print first line of opening message
;****************************************************************************************
printopening proc near                 ; Print first line of header
            push cs                    ; Save value of code segment
            pop ds                     ; Change default data segment to value of CS
            ;============================================================================
            seg es                     ; Override default with es segment
            mov al, [di+22]            ; Get drive letter supplied by dos
            add driveletter, al        ; Add drive letter to correct for printing
            ;============================================================================
            mov dx, offset return      ; Adress of cr lf pair
            mov ah, 09h                ; Dos function to print string
            int 21h                    ; Have DOS print string
            ;============================================================================
            mov ah, 08h                ; BIOS function to get video attribute
            mov bh, 00h                ; Use video page zero
            int 10h                    ; Use bios function to get video attrib
            mov byte vattrib, ah       ; Save video attribute
            ;============================================================================
            mov dx, offset drivename   ; First line of opening message
            mov bl, 74h                ; Print string in RED with a WHITE background
            call pstring               ; Call routine to print string at adress in DX
            mov dx, offset action      ; Adress of second line in opening message
            mov bl, 74h                ; Also RED on WHITE
            call pstring               ; Call routine to print string
            ;============================================================================
            ret                        ; Return to calling function
            endp
            ;============================================================================
vattrib     dw 0                       ; Screen attibute before we changed the screen
            ;============================================================================
drivename   db '     EXTENDED MEMORY RAM DRIVE'
return      db 0ah, 0dh, '$'
action      db '  Now installing as disk drive '
driveletter db 'A:', 0dh, 0ah, '$'

;****************************************************************************************
;   Print a string using the color code in BL then clear the next display line
;****************************************************************************************
pstring     proc near          ; Print string using the color code in BL
            mov ax, 0920h      ; Load function into AH and value of a space into AL
            xor bh, bh         ; Zero out BH
            mov cx, 35         ; Color only the first 35 charators of each line
            int 10h            ; Call BIOS function to color first part of display line
            ;============================================================================
            mov ah, 09         ; Print string located at adress in DX
            int 21h            ; DOS call to print string
            ;============================================================================
            mov ax, 0920h      ; Load function into AH and value of a space into AL
            mov bx, vattrib    ; Load zero into BH and old screen attribute into BL
            mov cx, 80         ; Load width of screen into CX
            int 10h            ; Call BIOS function to clear current display line
            ;============================================================================
            retn               ; Return to calling routine
            endp

;****************************************************************************************
;   Read possible user supplied options from the driver command line tail
;****************************************************************************************
readuservalues proc near       ; Read values supplied by user
            seg es             ; Use es as override segment
            lds si, [di+18]    ; Load ds and si with pointer
            ;============================================================================
getspace    lodsb              ; Get next letter - read past driver name
            cmp al, 0dh        ; Check for end of command tail
            jbe defaults       ; If end of command tail than use default values
            cmp al, ' '        ; Check to see if it is a space
            jne getspace       ; Loop until a space is found
            ;============================================================================
            call readavalue    ; Read first value on command line, this value is disksize
            jz defaults        ; If zero is not clear than there was no value
            call checkdisksize ; Check to insure that user supplied disk size is allowed
            ;============================================================================
            call readavalue    ; Read second value on command line, this is sector size
            jne dochksec       ; If zero continue using default values for disk info
            seg cs             ; Use CS because DS points to command line tail
            mov bx, bytespersec; Load BX with the default bytes per sector value
dochksec    call checksecsize  ; Check to insure that user supplied sector size is allowed
            ;============================================================================
            call readavalue    ; Third value would be number of entries in root directory
            jnz dochkroot      ; If zero then use the default value
            seg cs             ; Use CS because DS points to command line tail
            mov bx, rootdirents; Load BX with default number of directory entries
dochkroot   call checkrootdir  ; Check user supplied dir entry number
            ;============================================================================
            call calcfatsecs   ; Find the number of sectors needed by the FAT
defaults    retn               ; Return to calling routin
            endp

;****************************************************************************************
;   Read one value from the command line, if there is no value than set carry bit
;****************************************************************************************
readavalue  proc near          ; Read a value from the command line
            xor bx, bx         ; Clear bx because it will hold the read value
readspaces  lodsb              ; Load next digit into al
            cmp al, 20h        ; Check to see if digit is a space
            je readspaces      ; If it is a space then ignor it and read next digit
            ;============================================================================
checkdigit  cmp al, 20h        ; Check digit against a space
            je valueread       ; If it is a space than the value has been read
            cmp al, 0dh        ; Check digit against terminating digit
            jbe valueread      ; If it is the terminating digit than end routine
            sub al, 30h        ; Convert al from an ascii digit to a number
            jl verror          ; If less than zero than digit was not a number
            cmp al, 09h        ; Check if digit was greater than nine
            jg verror          ; If it was ten there was an error on command line
            cmp bx, 1996h      ; Check if the value is greater than 6550
            jae largeerror     ; If it is greater than jump to the value to large routine
            ;============================================================================
            xchg ax, bx        ; Retreive current value of string
            xor bh, bh         ; Zero out bh because it contains junk
            seg cs             ; Use CS rather then default DS
            mul ten            ; Multiply ax by ten
            add ax, bx         ; Add to the value the new number
            xchg ax, bx        ; Save current value
            lodsb              ; Get next digit
            jmps checkdigit    ; Check digit to insure it is a number
            ;============================================================================
largeerror  mov dx, offset tolargemess ; Load value to large error message
            jmps prnterror             ; Jump to the error handling routine
            ;============================================================================
verror      mov dx, offset verrormess  ; Adress of bad value error message
prnterror   pop cx                     ; Remove adress of return from stack
            pop cx                     ; Remove adress of calling routine from stack
            call backout               ; Call back out of installation
            ;============================================================================
valueread   dec si             ; Adjust pointer in si to point to last read value
            cmp bx, 0000h      ; Compare ax to zero, if zero than use defaults
            retn               ; Return to calling routine
            endp

;****************************************************************************************
;   Check the user supplied disk size to insure it is between 16k and 63mb
;****************************************************************************************
checkdisksize proc near        ; Check size of the user supplied disk
            cmp bx, 32         ; Check value against 18K
            jnb upperlimit     ; If it is smaller than check the upper limit
            mov bx, 32         ; If it was smaller then make it 18K
            ;============================================================================
upperlimit  cmp bx, 32767      ; Upper limit is 32767K
            jbe withinlimit    ; No problem so do nothing more
            mov bx, 32767      ; Value was larger so reduce it to maxs
            ;============================================================================
withinlimit seg cs             ; Use CS rather than default DS
            mov disksize, bx   ; Save disksize to ax
            ret                ; Return to calling routine
            endp

;****************************************************************************************
;   Check the user supplied sector size to insure it is either 128, 256 or 512
;****************************************************************************************
checksecsize proc near          ; Calculate size of sectors based on user supplied value
            mov cx, 128         ; Load smallest possible size into CX
            cmp bx, cx          ; Compare this size against user supplied size
            jle chksecsize      ; Jump if the size is equal to or smaller
            shl cx              ; Double size of possible sector in CX to 256 bytes
            cmp bx, cx          ; Compare this new size to user supplied size
            jle chksecsize      ; Jump if the size is equal to or smaller
            shl cx              ; Double size of the disk to 512, no need to check it
            ;============================================================================
chksecsize  seg cs              ; Use CS rather than default DS
            mov ax, disksize    ; Load AX with disk size in K
            xor dx, dx          ; Clear DX so it doesn't effect calculations
            div cx              ; Divide disksize by sector size
            cmp ax, 63          ; Check value against 63
            jle storesecsiz     ; Size is correct so store it
            shl cx              ; Double size of sectors
            jmps chksecsize     ; Check sector size again
storesecsiz seg cs              ; Use CS rather than default DS
            mov bytespersec, cx ; Save the sector size
            ;============================================================================
            seg cs              ; Use CS rather than default DS
            mov ax, disksize    ; Get disk size in K
            seg cs              ; Use CS rather than default DS
            mul onek            ; Multiply by 1024, the number of bytes per K
            div cx              ; Find total number of sectors on disk
            seg cs              ; Use CS rather than default DS
            mov sectors, ax     ; Save the total number of sector on the disk
            retn                ; Return to calling routine
            endp
            ;============================================================================
onek        dw 1024             ; The number of bytes in one K

;****************************************************************************************
;   Check the user supplied number of root directory entries to insure a correct number
;****************************************************************************************
checkrootdir proc near          ; Insure Root directory number is correct
            cmp bx, 512         ; Compare number of entries to max number allowed
            jbe findnumsecs     ; If less than find the number of sectors
            mov bx, 512         ; Reduce BX to max number of sectors
            ;============================================================================
findnumsecs seg cs              ; Use CS rather than default DS
            mov ax, bytespersec ; calculate number of sectors for the root directory
            seg cs              ; Use CS rather than default DS
            div thirtytwo       ; Each dir entry uses 32 bytes
            mov cl, al          ; CL now equals number of entries per sector
            mov ax, bx          ; Get requested number of entries for the disk
getsecs     div cl              ; Round down to sector size
            ;============================================================================
            cmp al, 00h         ; Check to insure at lest one sector is used for Root
            ja rightnumber      ; If not zero be certain number is not to big
            inc al              ; At lest one sector for root directory
rightnumber xor ah, ah          ; Clear high order byte
            ;============================================================================
storeroot   seg cs              ; Use CS rather than default DS
            mov secsinroot, ax  ; Save number of sectores for root directory
            mul cl              ; Find number of entires allowed number of sectors supports
            seg cs              ; Use CS rather than default DS
            mov rootdirents, ax ; Save number of entries
            retn                ; Return to calling routine
            endp
            ;============================================================================
thirtytwo   db 32      ; Number of bytes needed to hold one dir entry

;****************************************************************************************
;   Test if we must use either the 12 bit or 16 bit fat and calculate number of sectors
;****************************************************************************************
calcfatsecs proc near          ; Find number of sectors in the disks FAT table
            push cs            ; Save value of code segment
            pop ds             ; Put value of code segment into data segment
            ;============================================================================
            mov bp, sectors    ; Load into BP total number of sectors on disk
            sub bp, secsinroot ; Subtract the number of sectors in the Root directory
            dec bp             ; Subtract one for the boot sector
            ;============================================================================
            xor dx, dx         ; Clear DX so any stray values won't effect calculations
            mov ax, 1800h      ; Value used in calculating max number of secs in FAT
            div bytespersec    ; This number divided by the number of bytes in each
                               ;* sector and added to 4086 is the max number of sectors
                               ;* the disk can have before needing to use a 16 bit FAT
            push bp            ; Save number of sectors on disk minus boot and root secs
            sub bp, ax         ; Value left in BP is number of sectors used for data
            xor ax, ax         ; Clear AX, will hold number of sectors in FAT
            cmp bp, 4086       ; Check to decide if FAT will be 16 or 12 bits
                               ;* If greater than 4086 use a 16 bit FAT, this
                               ;* is given by DOS
            pop bp             ; Replace number of sectors on disk before FAT removed
            ja largefat        ; If greater then 4086 use 16 bit FAT
            ;============================================================================
smallfat    dec bp             ; Decrease total number of sectors
            inc ax             ; Increase number of sectors used by FAT
            push ax            ; Save number of sectors used by FAT
            mul bytespersec    ; Calculate number of bytes used by 12 bit FAT
            div three          ; Calculate number of sectors 12 bit FAT can support
            mul two            ; Div by 3 mul by 2 gives number of sectors supported
            cmp ax, bp         ; Compare the number of sectors this FAT can store
            pop ax             ; Retreive number of sectors used by 12 bit FAT
            jb smallfat        ; Continue with next if not enough!
            cmp bp, 4086       ; Check number of secs in FAT against DOS cut off number
            ja nocando         ; If number of sectors is still above 4086 then back out
            jmps endfat        ; Value is correct so end routine by storing fatsecs
            ;============================================================================
largefat    dec bp             ; Decrease total number of sectors
            inc ax             ; Increase number of sectors used by FAT
            push ax            ; Save number of sectors used by FAT
            mul bytespersec    ; Calculate number of bytes used by 16 bit FAT
            div two            ; Calculate number of sectors 16 bit FAT can support
            cmp ax, bp         ; Compare the number of sectors this FAT can store
            pop ax             ; Retreive number of sectors used by 16 bit FAT
            jb largefat        ; Continue with next if not enough!
            cmp bp, 4086       ; Check number of secs in FAT against DOS cut off number
            jbe nocando        ; If number of secs is below or equal disk is impossible
            mov byte fatsize, '6' ; Change value of DOS FAT size flag
            inc sizeoffat      ; Move 1 to FAT size flag
            ;============================================================================
endfat      mov fatsecs, ax    ; Save the number of sectors used in the FAT!
            retn               ; Return to calling routine
            ;============================================================================
nocando     mov dx, offset faterror ; Adress of error message
            pop cx             ; Remove adress of calling routine
            pop cx             ; Remove adress of routine that called calling routine
            call backout       ; Call backout to abort installation
            endp
            ;============================================================================
two         dw 2               ; Value used with several multiplications
three       dw 3               ; Value used with several multiplications
sizeoffat   db 0               ; If value is 0 FAT is 12 bits if 1 FAT is 16 bits

;****************************************************************************************
;   Try to allocate and reserve a block of xms memory large enough for the disk
;****************************************************************************************
getxmsblock proc near          ; Handle XMS memory needs for setup
            push cs            ; Push code segment
            pop ds             ; Pop DS to point default segment to code segment
            ;============================================================================
            mov ax, 4300h      ; Function call to test for an XMS driver
            int 2fh            ; Interupt to test for XMS driver
            cmp al, 80h        ; If AL equals 80h then the driver is loaded
            jne noxmsdriver    ; No driver so jump to error message handler
            ;============================================================================
            mov ax, 4310h      ; Function call to get XMS driver adress
            int 2fh            ; Make call to get XMS driver adress
            seg cs             ; Use CS rather than default DS
            mov xmsoffset, bx  ; Save offset of XMS adress
            seg cs             ; Use CS rather than default DS
            mov xmssegment, es ; Save segment of XMS adress
            mov ax, cs         ; Get value of code segment
            mov es, ax         ; Point ES to code segment
            ;============================================================================
            mov ah, 08h        ; Function to get size of largest free XMS block
            seg cs             ; Use code segment rather than data segment
            call far xmsadress ; Make call to XMS driver to find size of XMS block
            cmp ax, disksize   ; See if there is a large enough XMS block for the disk
            jb noroom          ; If there is no room than jump to error handler
            ;============================================================================
            mov ah, 09h        ; Function call to allocate a block of XMS memory
            mov dx, disksize   ; Amount of memory to allocate in K bytes
            seg cs             ; Use code segment rather than the data segment
            call far xmsadress ; Make call to the XMS driver
            dec ax             ; AX should contian 01 if the call was successful
            jnz strngerror     ; If AX was not one then jump to unexpected error routine
            seg cs             ; Use CS rather than default DS
            mov embhandle, dx  ; Save XMS handle returned by XMS driver
            retn               ; Return to calling routine
            ;============================================================================
noxmsdriver mov dx, offset noxmsmess  ; Adress of error message
            jmps leave                ; Leave this routine for the error handling routine
            ;============================================================================
noroom      mov dx, offset noroommess ; Adress of not enough XMS memory error message
            jmps leave                ; Goto the error handling routine
            ;============================================================================
strngerror  mov dx, offset strngmess  ; Adress of error message for unexpected XMS errors
leave       pop bx             ; Remove adress of calling routine from stack
            call backout       ; Call the routine to end installation and return to DOS
            endp
            ;============================================================================

;****************************************************************************************
;   Move the BIOS parm block to the disk boot sector, assume ES and DS point to CS
;****************************************************************************************
movebpbtosec proc near                 ; Move 25 bytes to disk boot sector
            mov si, offset bytespersec ; Adress of BPB
            mov di, offset sbpb        ; Adress of boot sector
            mov cx, 25                 ; Number of bytes to be moved
            rep                        ; Loop CX times
            movsb                      ; Move one byte CX times
            ret                        ; Return to calling program
            endp

;****************************************************************************************
;   Move the sector in CX to the disk
;****************************************************************************************
performmove proc near                 ; Move the boot sector using write_data routine
            seg cs                    ; Use CS rather than default DS
            mov iobuf, offset asector ; Move offset of asector to the iobuf pointer
            seg cs                    ; use CS rather than default DS
            mov iobuf+2, cs           ; Move the codesegment value to the iobuf pointer
            seg cs                    ; Use CS rather than default DS
            mov sectorcount, 01h      ; Only one sector being moved
            seg cs                    ; Use CS rather than default DS
            mov sectorstart, cx       ; Start with sector sector in CX
            push cs                   ; Move to AX the value of CS
            pop es                    ; Move to ES the value of CS by way to AX
            mov di, offset initrqh    ; Move to DI the offset of the fake RQH
            call write_data           ; Call routine that writes the data
            ret                       ; Return to calling routine
            endp

;****************************************************************************************
;   Move data to disk sectors starting with the Boot sector and working down from the Root
;****************************************************************************************
movesectors proc near        ; Move all sectors in the FAT to disk
            mov cx, 0h       ; Start by moving the boot sector
            call performmove ; Call the routine that does the moving
            jc moveerror     ; If carry is set then there was an error
            ;============================================================================
            push cs          ; Get value of the code segment
            pop es           ; Move value of the code segment into ES
            xor ax, ax       ; Clear out AX
            mov di, offset asector ; Load the adress of the sector into DI
            mov cx, 256      ; Loop 256 times, enough even if 512 bytes sectors are used
            rep              ; Loop CX times
            stosw            ; Move AX to adress ES:DI and inc DI
            ;============================================================================
            mov cx, fatsecs  ; Load into CX the number of sectors used by the FAT
            add cx, secsinroot ; Add to it the number of sectors used by the root
            ;============================================================================
moverootfat call performmove ; Move to the disk the sector number in CX
            jc moveerror     ; If there was an error than handle it
            loop moverootfat ; Continue looping until all sectors are written
            ;============================================================================
            mov word asector, 0fff8h    ; Store a full word at the begining of the sector
            mov byte asector+2, 0ffh    ; Store a byte
            ;============================================================================
            cmp byte sizeoffat, 1       ; Check to see if the FAT is 16 bits or 12 bits
            jnz mvfat                   ; If sizeoffat is 1 then disk uses 16 bit fat
            mov byte asector+3, 0ffh    ; Move this byte only for 16 bit FATs
            ;============================================================================
mvfat       mov cx, 01h                 ; Rewrite the first sector
            call performmove            ; Write the sector
            jc moveerror                ; Handle any errors
            mov si, offset vlabel       ; Adress of BPB
            mov di, offset asector      ; Adress of boot sector
            mov cx, 26                  ; Number of bytes to be moved
            rep                         ; Loop CX times
            movsb                       ; Move one byte CX times
            ;============================================================================
            mov cx, fatsecs             ; Get number of sectors used by the disk FAT
            inc cx                      ; Increase by one to account for the boot sector
            call performmove            ; Copy all sectors used by the FAT
            jc moveerror                ; If carry bit is set than backout with error
            ret                         ; Return to calling routine
            ;============================================================================
moveerror   mov dx, offset doserrormess ; Adress of DOS caused error
            pop bx                      ; Set stack to adress of main routine
            call backout                ; Exit through backout routine
            ;============================================================================
            endp

;****************************************************************************************
;   Pass to DOS needed information about the driver
;****************************************************************************************
givetodos   proc near               ; Give DOS end of used memory and pointer to bpb
            seg cs                  ; Use code segment rather than the default DS
            lds si, reqoffset       ; Pointer to where DOS wants the information put
            ;============================================================================
            mov byte [si+13], 01h   ; Only one drive controlled by this software
            ;============================================================================
            mov word [si+14], 1f0h  ; Give to DOS offset of end of memory used
            mov [si+16], cs         ; Give to DOS segment of memory used
            ;============================================================================
            mov ax, offset bpbptr   ; Get adress of pointer to BIOS parm block
            mov [si+18], ax         ; Give DOS offset of pointer to BIOS parm block
            mov [si+20], cs         ; Give DOS segment of pointer to BIOS parm block
            ret                     ; Return to calling routine
            endp

;****************************************************************************************
;   Print information found in boot sector of disk
;****************************************************************************************
printinfo   proc near                 ; Print user selectible information about disk
            push cs                   ; Load value of code segment into AX
            pop ds                    ; Point default data segment to value of CS
            push cs                   ; Save the value of the code segment
            pop es                    ; Point the extra segmet to the value of CS
            ;============================================================================
            mov di, offset diskbuff   ; Adress of first buffer
            mov dx, disksize          ; Get disksize in dx
            call convertdx            ; Convert value of dx to ASCII string
            mov bl, 30h               ; Print string in BLACK with a CYAN background
            mov dx, offset info       ; Load adress of string to print into DX
            call pstring              ; Print string showing disk size in K
            ;============================================================================
            mov di, offset sectorbuff ; Adress of sector size buffer
            mov dx, bytespersec       ; Get size of each sector
            call convertdx            ; Convert sector size in DX into ASCII string
            mov bl, 30h               ; Print string in BLACK with a CYAN background
            mov dx, offset str3       ; Load adress of string to be printed into DX
            call pstring              ; Print string showing sector size
            ;============================================================================
            mov di, offset entrbuff   ; Adress of root entrie buffer
            mov dx, rootdirents       ; Get number of entries in root director
            call convertdx            ; Convert root entrie number to ASCII string
            mov bl, 30h               ; Print string in BLACK on a CYAN background
            mov dx, offset str4       ; Load adress of string to be printed into DX
            call pstring              ; Print string showing Root directory entry number
            ;============================================================================
            mov bl, 71h               ; Print string in BLUE on a WHITE background
            mov dx, offset cpyrght    ; Load into DX the adress of the copyright message
            call pstring              ; Print the copyright message
            ;============================================================================
            mov dx, offset return     ; Load adress of CR/LF into DX
            mov ah, 09h               ; Load AH with DOS function to print a string
            int 21h                   ; Call to DOS to print CR/LF
            ;============================================================================
            seg cs                    ; Use code segment rather than default DS
            les di, reqoffset         ; Move to ES:DI pointer to DOS request header
            ret                       ; Return to calling function
            endp
            ;============================================================================
info        db ' Disk size in kilo bytes is       '
diskbuff    db 0ah, 0dh, '$'
str3        db ' Sector size in bytes is          '
sectorbuff  db 0ah, 0dh, '$'
str4        db ' Root directory file limit is     '
entrbuff    db 0ah, 0dh, '$'
cpyrght     db ' V1.1 Copyright ', 39, '92 Daniel Mendyke', 0ah, 0dh, '$'

;****************************************************************************************
;   Print 16 bit value as a ASCII string of digits
;****************************************************************************************
convertdx   proc near     ; Convert the value of dx to an ASCII string
            dec di        ; Adjust pointer by one byte down
            std           ; Set direction flag
            ;============================================================================
nextdigit   mov ax, dx    ; Get numerator
            xor dx, dx    ; Clear denomenator
            div ten       ; Divide by ten
            xchg ax, dx   ; Get quotient
            add al, 30h   ; Convert value into digit
            stosb         ; Move the byte in al to location [di]
            cmp dx, 0     ; Are we at and end?
            jnz nextdigit ; If not then do next digit
            ;============================================================================
            cld           ; Clear direction flag
            ret           ; Return to calling routine
            endp
            ;============================================================================
ten         dw 0ah        ; Value used by multiplication

;****************************************************************************************
;   Back out of the installation because of an error
;****************************************************************************************
backout     proc near          ; The installation failed so leave gracefully
            push cs            ; Store code segment
            pop ds             ; Point data segment to code segment
            mov bl, 30h        ; Print string in BLACK on a CYAN background
            call pstring       ; Print out string at adress in DX
            ;============================================================================
            mov dx, offset installstop ; Adress of intallation stopped message
            mov bl, 4fh        ; Print string in BRIGHT WHITE on a RED background
            call pstring       ; Print out string at adress in DX
            pop ax             ; Remove  adress of calling routine from stack
            mov ax, 8100h      ; Error bit set in status word
            ;============================================================================
            seg cs             ; Use code segment rather than data segment
            lds si, reqoffset  ; Load pointer to DOS request header
            mov word [si+14], 0; Load the zero as offset of the adress of memory used
            mov [si+16], cs    ; Load current code segment
            mov byte [si+13], 00h ; Load a zero to show that no drivers are remaining
            ;============================================================================
            stc                ; Set the carry bit so main routine knows of the error
            ret                ; Return to main routine, not calling routine
            endp
            ;============================================================================
verrormess   db '   Unrecognized charactor found!'  , 0ah, 0dh, '$'
tolargemess  db '     Value found is to large!!'    , 0ah, 0dh, '$'
faterror     db '     Can not use given values!'    , 0ah, 0dh, '$'
noroommess   db ' Not enough free extended memory!!', 0ah, 0dh, '$'
noxmsmess    db ' Extended memory driver not found!', 0ah, 0dh, '$'
strngmess    db ' Unexpected extended memory error!', 0ah, 0dh, '$'
doserrormess db '       Unexpected DOS error!'      , 0ah, 0dh, '$'
installstop  db '      Installation aborted!!!'     , 0ah, 0dh, '$'

;****************************************************************************************
;   Data needed only while driver is being loaded
;****************************************************************************************
disksize    dw 256        ; Size of disk in k
secsinroot  dw 0008h      ; Default number of sectors per root directory
            ;============================================================================
asector     db 'Archadious '
sbpb        ds 25, 0                   ; Space for Bios Parm Block
            dw 0                       ; Reserved by DOS
            db 29h                     ; Flag to show using extended boot info
            db '8888XMS-RAMdiskFAT1'   ; Volume label and FAT size
fatsize     db '2   '                  ; 16 or 12 bit FAT
            ds 64, 0
            dw 0aa55h       ; 128 bytes
            ds 126, 0
            dw 0aa55h       ; 256 bytes
            ds 254, 0
            dw 0aa55h       ; 512 bytes
            ;============================================================================
initrqh     ds 14, 0        ; Define 14 spare bytes
iobuf       dw 0            ; Offset of ASECTOR
            dw 0            ; Segment of ASECTOR
sectorcount dw 1            ; Number of sectors to be moved
sectorstart dw 0            ; Starting location of where sectors go in disk
            ;============================================================================
vlabel      db 'XMS-RAMdisk'; Disk's volume label
            db 28h          ; File attribute for a Volume Label
            ds 10, 0
ltime       dw 0940h        ; Volume label time is 1:10 am
ldate       dw 1630h        ; Volume label date is January 16, 1991


