// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.

(BEGIN)
@KBD
D=M

@BLACK
D;JNE

@WHITE
0;JMP

(BLACK)
@8191
D=A
@i
M=D

(LOOPB)
@i
D=M
@j
M=D
@SCREEN
D=A
@j
M=M+D
A=M
M=-1
@i
M=M-1
D=M
@LOOPB
D+1;JGT

@BEGIN
0;JMP

(WHITE)
@8191
D=A
@i
M=D

(LOOPW)
@i
D=M
@j
M=D
@SCREEN
D=A
@j
M=M+D
A=M
M=0
@i
M=M-1
D=M
@LOOPW
D+1;JGT

@BEGIN
0;JMP
