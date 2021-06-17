#include <print42.bas>

dim a, tecla as Ubyte
dim n as Uinteger
dim idioma as ubyte
dim dir as Uinteger
dim pascua as ubyte=0

REM Rotor,RingStellum,CurrentPos
DIM rotorSetup(2,2) as Ubyte = {{0,0,0},{1,0,0},{2,0,0}}
DIM plugBoard(19) as Ubyte={99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99}
DIM startingPos(2) as Ubyte

REM 8 rotors+reflector, 26 wires + notch. If Notch is 99=>its a double Notch (M and Z)
DIM rotorDefinition(8,26) as Ubyte = { _
       {24,16,18,4 ,12,13,5 ,22,7 ,14,3 ,21,2 ,23,24,19,14,10,13,6 ,8 ,1 ,25,12,2 ,20,0 }, _
       {4 ,9 ,10,2 ,7 ,1 ,23,9 ,13,16,3 ,8 ,2 ,9 ,10,18,7 ,3 ,0 ,22,6 ,13,5 ,20,4 ,10,17}, _
       {0 ,8 ,1 ,7 ,14,3 ,11,13,15,18,1 ,22,10,6 ,24,13,0 ,15,7 ,20,21,3 ,9 ,24,16,5 ,5 }, _
       {1 ,2 ,3 ,4 ,5 ,6 ,22,8 ,9 ,10,13,10,13,0 ,10,15,18,5 ,14,7 ,16,17,24,21,18,15,22}, _
       {4 ,17,12,18,11,20,3 ,19,16,7 ,10,23,5 ,20,9 ,22,23,14,1 ,13,16,8 ,6 ,15,24,2 ,10}, _
       {21,24,25,14,2,3,13,17,12,6,8,18,1,20,23,8,10,5,20,16,22,19,9,7,4,11,26}, _
       {9,14,4,18,10,15,6,24,16,7,17,19,1,20,11,2,13,19,8,25,3,16,12,5,21,23,99}, _
       {13,24,7,4,2,12,22,16,4,15,8,11,15,1,6,16,10,17,3,18,21,9,14,19,5,20,99}, _
       {5,9,14,4,15,6,17,7,20,18,25,7,3,16,11,2,10,21,12,3,19,13,24,1,8,22,99}} 

DIM inverseRotorDefinition(8,25) as Ubyte={ _
       {0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 }, _
       {6 ,5 ,4 ,23,4 ,2 ,1 ,18,13,10,9 ,7 ,10,3 ,2 ,22,9 ,20,0 ,8 ,3 ,13,9 ,7 ,10,16}, _
       {0 ,18,13,1 ,5 ,9 ,15,22,3 ,8 ,7 ,1 ,24,20,16,21,0 ,11,14,6 ,13,24,10,15,3 ,7 }, _
       {7 ,1 ,22,2 ,15,3 ,14,4 ,18,5 ,16,6 ,17,0 ,15,8 ,18,9 ,21,10,24,10,5 ,13,10,13}, _
       {19,2 ,6 ,8 ,4 ,14,13,20,23,3 ,16,22,15,23,12,11,7 ,5 ,17,1 ,10,18,24,9 ,16,20}, _
       {10,25,4 ,18,7 ,9 ,2 ,20,3 ,16,11,23,20,1 ,19,6 ,22,14,8 ,13,12,21,5 ,8 ,17,24}, _
       {8 ,17,5 ,13,19,24,4 ,20,12,9 ,19,16,6 ,1 ,10,14,7 ,2 ,25,21,15,18,23,3 ,16,11}, _
       {10,15,22,5 ,9 ,16,2 ,4 ,17,7 ,14,18,4 ,13,1 ,21,19,12,8 ,20,6 ,3 ,11,16,15,24}, _
       {10,18,20,16,12,5 ,8 ,4 ,13,25,9 ,6 ,21,19,7 ,3 ,14,2 ,7 ,15,24,22,3 ,17,1 ,11} _
       } 

DIM memory(249,2) as ubyte
DIM memoryPointer as integer

rem busca el principio de los datos
DIM charset as uinteger
buscaCharset()

inicializaPantalla()

Function codifica(byval c As ubyte) As ubyte
    Dim n,tipo,ring,rotPos,desp As Ubyte
    n=0
    Do
        if plugBoard(n*2)=c And plugBoard(n*2+1)<99
        c=plugBoard((n*2)+1)
        n=10
        end If
        if plugBoard(n*2+1)=c And plugBoard(n*2)<99 And n<99
        c=plugBoard(n*2)
        n=10
        end If
        n=n+1
    Loop While n<10
    
    tipo=rotorSetup(2,0)
    ring=rotorSetup(2,1)
    rotPos=rotorSetup(2,2)
    desp=rotorDefinition(tipo+1,(26+c+rotPos-ring) Mod 26)
    c=(c+desp) Mod 26
    
    tipo=rotorSetup(1,0)
    ring=rotorSetup(1,1)
    rotPos=rotorSetup(1,2)
    desp=rotorDefinition(tipo+1,(26+c+rotPos-ring) Mod 26)
    c=(c+desp) Mod 26
    
    tipo=rotorSetup(0,0)
    ring=rotorSetup(0,1)
    rotPos=rotorSetup(0,2)
    desp=rotorDefinition(tipo+1,(26+c+rotPos-ring) Mod 26)
    c=(c+desp) Mod 26
    
    desp=rotorDefinition(0,c Mod 26)
    c=(c+desp) Mod 26
    
    tipo=rotorSetup(0,0)
    ring=rotorSetup(0,1)
    rotPos=rotorSetup(0,2)
    desp=inverseRotorDefinition(tipo+1,(26+c+rotPos-ring) Mod 26)
    c=(26+c-desp) Mod 26
    
    tipo=rotorSetup(1,0)
    ring=rotorSetup(1,1)
    rotPos=rotorSetup(1,2)
    desp=inverseRotorDefinition(tipo+1,(26+c+rotPos-ring) Mod 26)
    c=(26+c-desp) Mod 26
    
    tipo=rotorSetup(2,0)
    ring=rotorSetup(2,1)
    rotPos=rotorSetup(2,2)
    desp=inverseRotorDefinition(tipo+1,(26+c+rotPos-ring) Mod 26)
    c=(26+c-desp) Mod 26
    
    n=0
    Do
        if plugBoard(n*2)=c And plugBoard(n*2+1)<99
        c=plugBoard((n*2)+1)
        n=10
        end If
        if plugBoard(n*2+1)=c And plugBoard(n*2)<99 And n<99
        c=plugBoard(n*2)
        n=10
        end If
        n=n+1
    Loop While n<10
    
    Return c
End Function

REM Bucle principal
bb$=""
idioma=0
while 1=1

aa$=inkey$
if aa$<>bb$ AND code(aa$)>0
	tecla=code (aa$)

	if tecla=12 and memoryPointer>0
		moveRotor(2,1)
		memoryPointer=memoryPointer-1
		click()
	end if

	if (tecla=49 OR tecla=50 OR tecla=51)
		pintaRingSet(0)
		changeRotor(tecla-49)
		saveStartingPos()
		click()
	end if
	if (tecla=52 OR tecla=53 OR tecla=54)
		pintaRingSet(0)	
		moveRotor(tecla-52,0)
		saveStartingPos()
		click()
	end if
	if (tecla=5)
		pintaRingSet(0)
		moveRotor(0,1)
		saveStartingPos()
		click()
	end if
	if (tecla=8)
		pintaRingSet(0)
		moveRotor(1,1)
		saveStartingPos()
		click()
	end if
	if (tecla=10)
		pintaRingSet(0)
		moveRotor(2,1)
		saveStartingPos()
		click()
	end if
	if tecla>=65 AND tecla<=90
		pintaRingSet(0)
		plugSetup(tecla-65)
		saveStartingPos()
		click()
	end if
	if tecla>=97 AND tecla <=122
		dim encoded as ubyte
		pintaRingSet(0)
		moveRotor(2,0)
		encoded=codifica(tecla-97)
		ilumina(encoded,1)
		click()
		do
		loop while tecla=code(inkey$)
		ilumina(encoded,0)
		memory(memoryPointer,0)=tecla-97
		memory(memoryPointer,1)=encoded
		memoryPointer=memoryPointer+1
		if memoryPointer>249
			saveStartingPos()
		end if

	end if
	if tecla=7 
		memoryPointer=0
		rotorSetup(0,1)=rotorSetup(0,1)+1
		if rotorSetup(0,1)>25 
			rotorSetup(0,1)=0
		end if
		pintaRingSet(1)
		beep 0.001,-5
		pausa (5000)
	end if
	if tecla=6 
		saveStartingPos()
		rotorSetup(1,1)=rotorSetup(1,1)+1
		if rotorSetup(1,1)>25 
			rotorSetup(1,1)=0
		end if
		pintaRingSet(1)
		beep 0.001,-5
		pausa (5000)

	end if
	if tecla=4
		saveStartingPos() 
		rotorSetup(2,1)=rotorSetup(2,1)+1
		if rotorSetup(2,1)>25 
			rotorSetup(2,1)=0
		end if
		pintaRingSet(1)
		beep 0.001,-5
		pausa (5000)
	end if

	if tecla=13

		
		border 0
		mainScreen(1)
		click()
		pausa(20000)
		paper 0
		ink 7

		do
		if idioma=0
			dir=charset+1224
		else
			dir=charset+1352
		end if
		poke uinteger 23675,dir
		
		if idioma=0
			print at 0,31;" "
			printat42 (0,0) :print42 "      EMULADOR MAQUINA ENIGMA       E=Eng "
			print at 1,31;" "
			printat42 (1,0) :print42 "      -----------------------             ":print at 1,27;"\A\B\C\D"
			print at 2,31;" "
			printat42 (2,0) :print42 "1,2,3 para cambiar rotores                ":print at 2,27;"\E\F\G\H"
			print at 3,31;" "
			printat42 (3,0) :print42 "CS+1,2,3 para ajustar Ringstellung        ":print at 3,27;"\I\J\K\L"
			print at 4,31;" "
			printat42 (4,0) :print42 "4,5,6 o CS+4,5,6 para rotar rotores       ":print at 4,27;"\M\N\O\P"
			print at 5,31;" "
			printat42 (5,0) :print42 "CS+[A..Z] para enchufar cables del panel  "
			print at 6,31;" "
			printat42 (6,0) :print42 "[a..z] Para (des)codificar y CS+0 deshacer"
			print at 7,31;" "
			printat42 (7,0) :print42 "SPACE para ver el historial               "
			print at 8,31;" "
			printat42 (8,0) :print42 "                                          "
			print at 23,31;" ";
			printat42 (23,0):print42 "(c)2021 @setaseta - tests por @desUBIKado "
		else
			print at 0,31;" "
			printat42 (0,0) :print42 "      ENIGMA MACHINE EMULATOR       E=Esp "
			print at 1,31;" "
			printat42 (1,0) :print42 "      -----------------------             ":print at 1,27;"\A\B\C\D"
			print at 2,31;" "
			printat42 (2,0) :print42 "To select rotors: press 1,2,3             ":print at 2,27;"\E\F\G\H"
			print at 3,31;" "
			printat42 (3,0) :print42 "To set Ringstellung: press CS+1,2,3       ":print at 3,27;"\I\J\K\L"
			print at 4,31;" "
			printat42 (4,0) :print42 "To set Plugboard: press CS+[A..Z]         ":print at 4,27;"\M\N\O\P"
			print at 5,31;" "
			printat42 (5,0) :print42 "To rotate rotors: press 4,5,6 or CS+4,5,6 "
			print at 6,31;" "
			printat42 (6,0) :print42 "To encode/decode: press [a..z] & CS+0:UNDO"
			print at 7,31;" "
			printat42 (7,0) :print42 "To see your encoding records: press SPACE "
			print at 8,31;" "
			printat42 (8,0) :print42 "                                          "
			print at 23,31;" ";
			printat42 (23,0):print42 "(c)2021 @setaseta - tested by @desUBIKado "
		endif
	    
	    do
		loop while code(inkey$)=13
		
		do
		loop while inkey$=""
		
	    if code(inkey$)=101 
	    	idioma=(idioma+1) mod 2
	    end if

	    loop while code(inkey$)=101
				
		inicializaPantalla()
		click()
	end if
	
	if tecla=32
	
	border 6:paper 6: ink 0: cls

rot$=""
for n=0 to 2
if rotorSetup(n,0)=0
	rot$=rot$+"I "
elseif rotorSetup(n,0)=1
	rot$=rot$+"II "
elseif rotorSetup(n,0)=2
	rot$=rot$+"III "
elseif rotorSetup(n,0)=3
	rot$=rot$+"IV "
elseif rotorSetup(n,0)=4
	rot$=rot$+"V "
elseif rotorSetup(n,0)=5
	rot$=rot$+"VI "
elseif rotorSetup(n,0)=6
	rot$=rot$+"VII "
elseif rotorSetup(n,0)=7
	rot$=rot$+"VII "
end if
next n	
printat42(0,0):  print42 "Privat:      Walzenlage      Ringstellung"
printat42(1,14):  print42 rot$ 
printat42(1,17+14): print42 (str(rotorSetup(0,1)+1))
printat42(1,20+14): print42 (str(rotorSetup(1,1)+1))
printat42(1,23+14): print42 (str(rotorSetup(2,1)+1))
printat42(4,0): print42 (str(memoryPointer)+"/250")
off=chr(startingPos(0)+65)+chr(startingPos(1)+65)+chr(startingPos(2)+65)
printat42(1,1): print42 off

printat42(2,0): print42 "             ---- Steckerverbindungen ----"                         
for n=0 to 9

	if plugBoard(n*2)=99
		char1="."
	else
		char1=chr(plugBoard(n*2)+65)
	end if

	if plugBoard(n*2+1)=99
		char2="."
	else
		char2=chr(plugBoard(n*2+1)+65)
	end if
	
	printat42(3,n*3+12): print42(char1)
	printat42(3,n*3+13): print42(char2)

next n

printat42(14,0): print42 "Offentlich: "

dim e as uinteger
dim q as ubyte
dim w as ubyte
e=0
q=0
w=0

plot 0,82:draw 255,0


while (e*35+q*5+w<memoryPointer)
	printat42(e+5,q*6+w): print42 chr(memory(e*35+q*5+w,0)+65)
	printat42(e+15,q*6+w): print42 chr(memory(e*35+q*5+w,1)+65)
	w=w+1
	if w>4
		w=0
		q=q+1
		if q>6
			q=0
			e=e+1
		end if
	end if
end while
	
	do
	loop while code(inkey$)=32
	do
	loop while inkey$=""

	inicializaPantalla()
	
	end if

	if rotorSetup(0,0)=1 and rotorSetup(1,0)=3 and rotorSetup(2,0)=4
		
		if rotorSetup(0,1)=1 and rotorSetup(1,1)=20 and rotorSetup(2,1)=11

			if rotorSetup(2,2)=0 and pascua=0
				paper 1 
				ink 6	
				bright 1
				flash 1
				Printat42(0,0)
				Print42 ("    Wikipedia fan detected. Congrats!!!   ")
				Printat42(23,0)
				Print42 ("     Please tell @setaseta on twitter     ")
				pausa(50000)
				do
				loop while inkey$=""
					inicializaPantalla()
				pascua=1
			end if
		end if	
	end if
end if
bb$=aa$

end while

sub saveStartingPos()
	startingPos(0)=rotorSetup(0,2)
	startingPos(1)=rotorSetup(1,2)
	startingPos(2)=rotorSetup(2,2)
	memoryPointer=0


end sub


sub inicializaPantalla()
	 
	border 7 
	bright 0
	flash 0	
	mainScreen(0)

	for n=0 to 9
		pintaPlug(n,0)
	next n	
	
	letra(5,4,rotorSetup(0,2),0)
	letra(10,4,rotorSetup(1,2),0)
	letra(15,4,rotorSetup(2,2),0)
	
	letra(5,1,26+rotorSetup(0,0),2)
	letra(10,1,26+rotorSetup(1,0),2)
	letra(15,1,26+rotorSetup(2,0),2)

	pintaPlugs()
	pintaLetrasPlugs()
	
end sub

sub ilumina (byval letra as ubyte,byval modo as ubyte)
	dim filAtt1,filAtt2 as ubyte

	if modo=0
		filAtt1=7
		filAtt2=71
	else
		filAtt1=70
		filAtt2=70
	end if

	dir=22528
	16

	DIM germanLayout(25) as ubyte={9,22,20,11,2,12,13,14,7,15,16,25,24,23,8,17,0,3,10,4,6,21,1,19,18,5}
		
	letra=germanLayout(letra)

	if letra>=0 and letra<9
		dir=dir+letra*3+323
	end if
	
	if letra>=9 and letra<17
		letra=letra-9
		dir=dir+letra*3+388
	end if	

	if letra>=17 and letra<26
		letra=letra-17
		dir=dir+letra*3+451
	end if

	poke dir   ,filAtt1
	poke dir+1 ,filAtt1
	poke dir+32,filAtt2
	poke dir+33,filAtt2
end sub

sub pintaRingSet(byval modo as ubyte)

for n=0 to 2
	if modo=0 
		aa$="  "
	else
		aa$=str(rotorSetup(n,1)+1)
		if len(aa$)=1
			aa$=aa$+" "
		end if
	end if
	ink 7
	paper 1
	bright 0
	printat42(7,7+n*7)
	print42(aa$)
next n

end sub

sub pausa(byval tiempo as uinteger)
for n=0 to tiempo
	poke 0,0
next n
end sub

sub pintaPlugs()
for n=0 to 9 
if plugBoard(n*2)<99 AND plugBoard(n*2+1)<99
	pintaPlug (n,1)
else
	pintaPlug (n,0)
end if
next n
end sub


sub pintaLetrasPlugs()
for n=0 to 9 
if plugBoard(n*2)<99
	print at 18,n*2+2;over 1;paper 1;bright 0;ink 6;chr(65+plugBoard(n*2)) 
end if 
if plugBoard(n*2+1)<99
	print at 22,n*2+2;over 1;paper 1;bright 0;ink 6;chr(65+plugBoard(n*2+1)) 
end if
next n
end sub


sub plugSetup(byval plug as ubyte)
dim l as ubyte
dim ex as ubyte
ex=0

for n=0 to 19
if plugBoard(n)=plug
	plugBoard(n)=99
	if n=0 OR n=2 OR n=4 OR n=6 OR n=8 OR n=10 OR n=12 OR n=14 OR n=16 OR n=18 
		print at 18,2+n;over 1;paper 1;ink 6;chr(65+plug)
	else
		print at 22,2+n-1;over 1;paper 1;ink 6;chr(65+plug)
	end if
	ex=1
end if
next n

if ex=0
	l=0
	do
	if plugBoard(l)=99
		plugBoard(l)=plug
		if l=0 OR l=2 OR l=4 OR l=6 OR l=8 OR l=10 OR l=12 OR l=14 OR l=16 OR l=18 
			print at 18,2+l;over 1;paper 1;ink 6;chr(65+plug)
		else
			print at 22,2+l-1;over 1;paper 1;ink 6;chr(65+plug)
		end if
		ex=1
	end if	
	l=l+1
	loop until (ex=1 OR l=20) 
end if 

pintaPlugs()

end sub

sub changeRotor(byval rotor as ubyte)
	DO
		rotorSetup(rotor,0)= rotorSetup(rotor,0)+1
		if rotorSetup(rotor,0)>=8
			rotorSetup(rotor,0)=0
		end if
	LOOP UNTIL NOT(rotorSetup(0,0)=rotorSetup(1,0) OR rotorSetup(0,0)=rotorSetup(2,0) OR rotorSetup(1,0)=rotorSetup(2,0))
	letra(5+rotor*5,1,rotorSetup(rotor,0)+26,2)
end sub

sub moveRotor(byval rotor as ubyte, byval move as ubyte)
	 	dim notch1,notch2 as ubyte


	 	if rotor>0 
	 	if rotorSetup(rotor,0)>4
	 		notch1=26+move
	 		notch2=13+move
	 	else
	 		notch1=rotorDefinition(rotorSetup(rotor,0)+1,26)+move
	 		notch2=notch1
	 	endif
	 	if notch1=rotorSetup(rotor,2)+1 OR notch2=rotorSetup(rotor,2)+1
			moveRotor(rotor-1,move)
		end if
		end if
	 	if move=0
		rotorSetup(rotor,2)=rotorSetup(rotor,2)+1
		if rotorSetup(rotor,2)=26
			rotorSetup(rotor,2)=0
		end if
		dim anterior as byte
		anterior=rotorSetup(rotor,2)-1
		if anterior=255
			anterior=25
		end if
		letra(5+rotor*5,4,anterior,1)
		pintaRotor(rotor,1)
		pausa (1000)
		letra(5+rotor*5,4,rotorSetup(rotor,2),0)
		pintaRotor(rotor,0)
		end if



		if move=1
		rotorSetup(rotor,2)=rotorSetup(rotor,2)-1
		if rotorSetup(rotor,2)=255
			rotorSetup(rotor,2)=25
		end if	
		dim sig as byte
		sig=rotorSetup(rotor,2)+1
		if sig=26
			sig=0
		end if
		letra(5+rotor*5,4,sig,1)
		pintaRotor(rotor,1)
		pausa (1000)
		letra(5+rotor*5,4,rotorSetup(rotor,2),0)
		pintaRotor(rotor,0)

		end if
		
end sub


sub pintaPlug(byval x as ubyte, byval value as ubyte)
	dim dir as uinteger
	dir = charset+96
	poke uinteger 23675,dir

	if value=0
		bright 1
		paper 0
		ink 6
		print at 19,x*2+2;"\A"
		print at 21,x*2+2;"\A"
		bright 0
		paper 1
		print at 20,x*2+2;"\B"
	else
		paper 2
		ink 6
		bright 1
		print at 19,x*2+2;"\C"
		print at 21,x*2+2;"\E"
		paper 1
		ink 2
		print at 20,x*2+2;"\D"
	end if
end sub


sub letra (byval x as ubyte,y as ubyte, letra as ubyte, byval media as ubyte)
	dim dir as uinteger
	dir=32*cast(integer,letra)+charset+96+40
	poke uinteger 23675,dir
	over 0:paper 7:ink 0:bright 1
	if media=0
		print at y,x;"\A\B"
		bright 0
		print at y+1,x;"\C\D"
	end if 
	if media=1
		print at y,x;"\C\D"
		bright 0
		print at y+1,x;"\E\F"	
	end if
	if media=2
		paper 1
		ink 2
		bright 0
		print at y,x;"\A\B"
		print at y+1,x;"\C\D"
		
	end if 
	over 0
end sub

sub pintaRotor(byval pos as ubyte, byval tipo as ubyte)
	dir=charset
	poke uinteger 23675,dir
	dim x as ubyte
	x=pos*5+3
	paper 0
	ink 5
	bright 0
	if tipo=0 
		print at 2,x;"\A"
		print at 3,x;"\B"
		print at 4,x;"\C"
		print at 5,x;"\D"
		print at 6,x;"\E"
		print at 7,x;"\F"
	end if	
	if tipo=1
		print at 2,x;"\G"
		print at 3,x;"\H"
		print at 4,x;"\I"
		print at 5,x;"\J"
		print at 6,x;"\K"
		print at 7,x;"\L"
	end if
end sub


datos:
asm
defb 2,1,3,8
charset:
defb 60,0,60,189,60,129,60,189
defb 60,189,60,189,0,189,60,189
defb 60,189,60,189,60,189,60,129
defb 60,189,60,189,60,189,60,189
defb 60,189,60,129,60,189,60,189
defb 60,189,0,189,60,60,0,60
defb 60,0,60,189,0,189,60,189
defb 0,189,60,189,60,189,60,189
defb 0,189,60,189,60,189,60,189
defb 60,189,60,189,60,189,60,189
defb 0,189,60,189,60,189,60,189
defb 0,189,60,189,0,60,60,0
defb 255,195,129,129,129,129,195,255
defb 126,0,0,0,0,0,0,126
defb 255,195,129,129,129,129,195,195
defb 60,60,60,60,60,60,60,60
defb 195,195,129,129,129,129,195,255
defb 0,0,0,0,7,15,12,12, 0,0,0,0,224,240,48,48, 15,15,12,12,0,0,0,0,240,240,48,48,0,0,0,0,0,0,0,0,15,15,12,15,0,0,0,0,224,240,48,224,15,12,15,15,0,0,0,0,240,48,240,224,0,0,0,0,0,0,0,0,7,15,12,12,0,0,0,0,224,224,0,0,12,12,12,7,7,0,0,0,0,0,0,224,224,0,0,0,0,0,0,0,15,15,12,12,0,0,0,0,192,224,112,48,12,12,15,15,0,0,0,0,48,112,224,192,0,0,0,0,0,0,0,0,15,15,12,15,0,0,0,0,240,240,0,224,15,12,15,15,0,0,0,0,224,0,240,240,0,0,0,0,0,0,0,0,15,15,12,15,0,0,0,0,240,240,0,224,15,12,12,12,0,0,0,0,224,0,0,0,0,0,0,0,0,0,0,0,7,15,12,12,0,0,0,0,224,224,0,0,12,12,15,7,0,0,0,0,48,48,240,240,0,0,0,0,0,0,0,0,12,12,12,15,0,0,0,0,48,48,48,240,15,12,12,12,0,0,0,0,240,48,48,48,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,128,128,128,128,1,1,1,1,0,0,0,0,128,128,128,128,0,0,0,0,0,0,0,0,15,15,0,0,0,0,0,0,240,240,48,48,0,12,15,7,0,0,0,0,48,48,240,224,0,0,0,0,0,0,0,0,12,12,12,15,0,0,0,0,48,112,224,192,15,12,12,12,0,0,0,0,192,224,112,48,0,0,0,0,0,0,0,0,12,12,12,12,0,0,0,0,0,0,0,0,12,12,15,15,0,0,0,0,0,0,240,240,0,0,0,0,0,0,0,0,12,14,15,15,0,0,0,0,48,112,240,240,13,13,13,12,0,0,0,0,176,176,176,48,0,0,0,0,0,0,0,0,12,14,15,15,0,0,0,0,48,48,48,176,13,12,12,12,0,0,0,0,240,240,112,48,0,0,0,0,0,0,0,0,7,15,12,12,0,0,0,0,224,240,48,48,12,12,15,7,0,0,0,0,48,48,240,224,0,0,0,0,0,0,0,0,15,15,12,12,0,0,0,0,224,240,48,48,15,15,12,12,0,0,0,0,240,224,0,0,0,0,0,0,0,0,0,0,7,15,12,12,0,0,0,0,224,240,48,48,12,12,15,7,0,0,0,0,48,112,224,208,0,0,0,0,0,0,0,0,15,15,12,12,0,0,0,0,224,240,48,48,15,15,12,12,0,0,0,0,240,224,112,48,0,0,0,0,0,0,0,0,7,15,12,15,0,0,0,0,240,240,0,224,7,0,15,15,0,0,0,0,240,48,240,224,0,0,0,0,0,0,0,0,15,15,1,1,0,0,0,0,240,240,128,128,1,1,1,1,0,0,0,0,128,128,128,128,0,0,0,0,0,0,0,0,12,12,12,12,0,0,0,0,48,48,48,48,12,12,15,7,0,0,0,0,48,48,240,224,0,0,0,0,0,0,0,0,12,12,12,12,0,0,0,0,48,48,48,48,14,7,3,1,0,0,0,0,112,224,192,128,0,0,0,0,0,0,0,0,12,13,13,13,0,0,0,0,48,176,176,176,15,15,14,12,0,0,0,0,240,240,112,48,0,0,0,0,0,0,0,0,12,14,7,3,0,0,0,0,48,112,224,192,3,7,14,12,0,0,0,0,192,224,112,48,0,0,0,0,0,0,0,0,12,12,12,15,0,0,0,0,48,48,48,240,7,1,1,1,1,0,0,0,224,128,128,128,128,0,0,0,0,0,0,0,15,15,0,3,0,0,0,0,240,240,112,224,7,14,15,15,0,0,0,0,192,0,240,240,0,0,0,0

defb 0,7,1,1,1,1,1,1,0,224,128,128,128,128,128,128,1,1,1,1,1,1,7,0,128,128,128,128,128,128,224,0
defb 0,31,6,6,6,6,6,6, 0,248,96,96,96,96,96,96,6,6,6,6,6,6,31,0,96,96,96,96,96,96,248,0
defb 0,63,13,13,13,13,13,13,0,252,176,176,176,176,176,176,13,13,13,13,13,13,63,0,176,176,176,176,176,176,252,0
defb 0,63,13,13,13,13,13,12,0,252,152,152,152,152,152,240,12,12,12,12,12,12,31,0,240,240,240,96,96,96,252,0
defb 0,31,6,6,6,6,6,3,0,248,96,96,96,96,96,192,3,3,3,1,1,1,7,0,192,192,192,128,128,128,224,0
defb 0,63,12,12,12,12,12,7,0,254,216,216,216,216,216,152,7,7,7,3,3,3,15,0,152,152,152,24,24,24,252,0
defb 0,255,51,51,51,51,51,30,0,254,108,108,108,108,108,108,30,30,30,12,12,12,63,0,108,108,108,108,108,108,254,0
defb 0,255,103,103,103,103,103,61,0,255,182,182,182,182,182,182,61,61,61,25,25,25,127,0,182,182,182,182,182,182,255,0


defb 0,0,0,31,16,23,23,23,0,0,0,201,71,93,78,68,0,0,0,39,228,101,181,53,0,0,0,240,16,208,208,208,16,31,0,29,7,15,26,0,72,213,29,246,54,104,146,97,116,87,0,140,158,60,140,194,16,240,0,64,32,128,48,112,7,22,11,22,0,31,16,23,163,223,13,250,18,216,94,93,225,32,0,95,241,21,81,255,0,176,0,80,32,176,176,64,23,23,16,31,0,0,0,0,71,83,89,215,0,0,0,0,177,118,187,97,0,0,0,0,128,144,208,48,0,0,0,0

defb 0,0,0,31,16,23,23,23,0,0,0,221,92,86,90,64,0,0,0,231,52,69,229,229,0,0,0,240,16,208,208,208,16,31,0,25,30,14,7,23,83,213,9,194,141,67,182,245,148,87,144,18,243,209,158,80,16,240,0,240,144,48,112,48,29,7,6,26,0,31,16,23,144,224,29,82,21,199,82,85,204,193,128,31,17,117,145,255,176,0,64,16,144,0,240,0,23,23,16,31,0,0,0,0,76,76,89,211,0,0,0,0,156,27,41,243,0,0,0,0,48,32,144,112,0,0,0,0

end asm

sub mainScreen(screen as ubyte)

asm
; Taken from 
; https://tomdalby.com/other/lzf.html
ld hl,data
cp 0
jr z, screen2
ld hl,data2
screen2:
ld de,22528 ; fixed at attribute 0x,0y
call _uclzf020
jp endScreen

;;
;; full screen decompression 80bytes long
_uclzf010:
call _uclzf100
inc hl
djnz _uclzf010
_uclzf020:
ld a,(hl)
inc hl
ld b,a
inc b
ret z
cp 32
jr c,_uclzf010
ld c,a
and 224
rlca
rlca
rlca
cp 7
jr nz,_uclzf030
add a,(hl)
inc hl
_uclzf030:
add a,2
ld b,a
push hl
ld a,c
and 31
add a,64
ld l,(hl)
ld h,a
_uclzf080:
call _uclzf100
ex de,hl
call _uclzf110
ex de,hl
djnz _uclzf080
pop hl
inc hl
jr _uclzf020
_uclzf100:
ld a,(hl)
ld (de),a
_uclzf110:
ld a,d
cp 88
jr c,_uclzf120
rlca
rlca
rlca
xor 130
ld d,a
ret
_uclzf120:
inc a
ld d,a
and 7
ret nz
xor d
rra
rra
rra
add a,79
ld d,a
inc de
ret

data:
	defb $01,$38,$00,$40,$00,$02,$03,$07,$07,$78,$00,$00,$ff,$24,$01,$f8
	defb $f7,$01,$e3,$01,$01,$05,$00,$c0,$e0,$e0,$38,$0f,$a0,$20,$02,$08
	defb $80,$00,$81,$21,$01,$08,$00,$e1,$06,$21,$02,$ff,$08,$00,$e1,$0f
	defb $21,$e7,$52,$21,$00,$0e,$e0,$08,$30,$00,$0f,$a1,$21,$01,$01,$4f
	defb $a1,$23,$00,$00,$f8,$4f,$34,$03,$00,$80,$38,$f0,$a0,$3f,$f8,$01
	defb $20,$e0,$00,$22,$00,$01,$a0,$42,$0b,$05,$3c,$00,$3c,$bd,$3c,$81
	defb $3c,$bd,$08,$80,$c0,$80,$44,$f8,$0a,$24,$e0,$50,$42,$00,$0e,$e0
	defb $08,$24,$01,$0f,$02,$a0,$53,$04,$08,$0f,$3f,$7f,$7f,$44,$01,$01
	defb $08,$ff,$a0,$55,$00,$28,$a0,$55,$01,$fc,$68,$82,$56,$01,$80,$00
	defb $78,$57,$00,$f8,$20,$00,$78,$57,$58,$33,$98,$57,$00,$1f,$c6,$59
	defb $23,$5a,$f8,$00,$55,$04,$08,$f0,$fc,$fe,$fe,$44,$01,$01,$4f,$40
	defb $a0,$5e,$f8,$1d,$3f,$03,$bd,$3c,$bd,$00,$21,$63,$b8,$44,$01,$81
	defb $c3,$b8,$22,$26,$54,$c0,$65,$b8,$42,$01,$81,$c1,$f8,$49,$63,$a1
	defb $3e,$00,$c0,$f8,$09,$52,$b8,$56,$00,$fe,$47,$56,$0b,$fc,$f0,$c0
	defb $00,$01,$07,$68,$e0,$00,$00,$07,$0f,$23,$54,$0d,$68,$00,$03,$f3
	defb $fd,$fd,$bd,$3d,$3d,$68,$c7,$cf,$df,$de,$43,$78,$05,$68,$f9,$fd
	defb $fd,$3d,$3d,$20,$42,$0a,$68,$80,$f8,$ff,$ff,$df,$df,$dd,$dd,$68
	defb $03,$25,$71,$0c,$e0,$e7,$ef,$ef,$68,$ff,$7f,$1f,$07,$01,$80,$e0
	defb $f0,$b8,$5b,$01,$3f,$1f,$f8,$29,$5e,$42,$43,$05,$08,$83,$c3,$83
	defb $c3,$a3,$23,$84,$00,$78,$c0,$22,$f8,$00,$85,$02,$08,$c1,$e1,$80
	defb $87,$f8,$49,$83,$01,$c0,$e0,$80,$91,$f8,$0a,$72,$0b,$f8,$f0,$f2
	defb $f5,$f3,$f0,$f8,$fc,$28,$0f,$0f,$0e,$60,$95,$04,$28,$cf,$0f,$0f
	defb $cf,$20,$96,$02,$ef,$68,$3d,$a0,$97,$00,$68,$63,$78,$45,$78,$20
	defb $42,$06,$fd,$fd,$fd,$1d,$1d,$68,$dd,$a0,$9a,$06,$68,$de,$dc,$dc
	defb $dd,$df,$df,$26,$56,$60,$3f,$0b,$e0,$e0,$e0,$68,$0f,$0f,$47,$a7
	defb $67,$07,$0f,$17,$f8,$2a,$7e,$45,$63,$44,$84,$04,$a3,$d3,$a3,$d3
	defb $38,$c0,$22,$00,$38,$e0,$10,$86,$e7,$41,$a3,$c0,$91,$00,$0e,$e0
	defb $09,$72,$63,$5d,$45,$55,$04,$07,$01,$c0,$f0,$fc,$85,$55,$06,$37
	defb $07,$00,$00,$e0,$fc,$28,$60,$97,$03,$1d,$00,$00,$28,$43,$78,$05
	defb $df,$cf,$cf,$00,$68,$1d,$20,$b9,$03,$fd,$fd,$f8,$00,$98,$9a,$00
	defb $d8,$26,$b7,$15,$fc,$fc,$fc,$c0,$80,$00,$03,$3c,$28,$e0,$80,$01
	defb $06,$19,$e7,$1f,$ff,$28,$2f,$5f,$bf,$7f,$e4,$2c,$5d,$25,$43,$46
	defb $a3,$03,$d1,$a1,$d0,$a0,$45,$44,$02,$ff,$ff,$55,$83,$21,$e0,$00
	defb $c5,$02,$c1,$a1,$41,$e3,$4e,$c2,$02,$c0,$a0,$40,$63,$31,$f8,$09
	defb $b2,$78,$55,$03,$7f,$7f,$3f,$0f,$78,$5c,$e4,$05,$54,$01,$c0,$fe
	defb $a2,$55,$22,$01,$21,$7a,$86,$d7,$63,$01,$03,$28,$03,$7c,$83,$63
	defb $54,$02,$28,$c3,$3f,$e2,$05,$55,$87,$54,$03,$fe,$fe,$fc,$f0,$f8
	defb $27,$5e,$24,$63,$20,$43,$f8,$1d,$44,$e1,$07,$e3,$00,$0d,$c0,$22
	defb $00,$0d,$e0,$2c,$e6,$f8,$0a,$31,$00,$01,$c1,$32,$25,$34,$e3,$4e
	defb $f3,$c0,$21,$f8,$0f,$3f,$25,$00,$78,$22,$84,$54,$22,$c5,$64,$54
	defb $40,$21,$24,$54,$87,$23,$ec,$0e,$04,$e8,$51,$03,$e8,$67,$0f,$64
	defb $1f,$e0,$09,$3f,$c0,$20,$03,$47,$00,$1f,$20,$60,$5e,$00,$47,$c0
	defb $f4,$f9,$da,$23,$01,$f8,$04,$83,$53,$01,$f8,$f4,$88,$3e,$f9,$13
	defb $1f,$c0,$5e,$0e,$07,$07,$1f,$3f,$7f,$78,$f0,$f3,$f3,$07,$e0,$f8
	defb $fc,$fe,$1e,$22,$96,$00,$47,$c0,$22,$79,$43,$03,$73,$f2,$f2,$f2
	defb $79,$44,$03,$ce,$4f,$4f,$4f,$f9,$05,$45,$03,$70,$f0,$f3,$f0,$79
	defb $44,$03,$0e,$0f,$ff,$1f,$f9,$08,$48,$ef,$0f,$43,$03,$70,$f0,$fe
	defb $fe,$b9,$4a,$01,$7f,$7f,$f9,$07,$48,$01,$ff,$fc,$b9,$4a,$00,$8f
	defb $ef,$06,$4a,$01,$73,$f3,$ae,$43,$01,$ce,$cf,$ee,$07,$44,$01,$7e
	defb $fe,$ae,$4f,$01,$7e,$7f,$ee,$07,$50,$ec,$05,$43,$20,$53,$eb,$29
	defb $3d,$11,$47,$f3,$f3,$f0,$78,$7f,$3f,$1f,$07,$47,$4f,$8f,$1f,$2e
	defb $fe,$fc,$f8,$e0,$23,$b6,$aa,$23,$03,$f0,$f0,$f1,$73,$6c,$63,$03
	defb $0f,$0f,$8f,$ce,$ec,$06,$64,$02,$f3,$f0,$70,$6c,$63,$03,$1f,$ff
	defb $0f,$0e,$ec,$07,$64,$00,$f3,$ab,$66,$00,$1f,$ea,$07,$67,$2d,$58
	defb $00,$7e,$6c,$63,$2d,$59,$00,$7e,$ec,$05,$64,$01,$f8,$f1,$aa,$69
	defb $00,$3f,$e9,$08,$6a,$e8,$00,$63,$23,$96,$00,$1e,$ec,$20,$6d,$e8
	defb $08,$75,$f9,$1b,$5d,$00,$10,$e8,$09,$41,$e8,$12,$5a,$e8,$07,$5a
	defb $ef,$18,$49,$04,$3e,$1f,$8f,$cf,$05,$e8,$10,$48,$01,$7f,$05,$e8
	defb $0e,$5a,$02,$1f,$ff,$ff,$f9,$08,$54,$8f,$49,$2c,$56,$2f,$21,$e9
	defb $06,$51,$00,$ff,$b9,$4a,$2e,$44,$e8,$0e,$92,$02,$8f,$1f,$3f,$f9
	defb $00,$8c,$f9,$00,$8c,$f9,$00,$5d,$00,$10,$e8,$11,$3e,$00,$30,$e8
	defb $11,$81,$f9,$01,$6c,$21,$96,$eb,$06,$67,$01,$f8,$ff,$aa,$69,$00
	defb $0f,$e9,$08,$76,$28,$63,$8b,$69,$03,$cf,$8f,$1f,$3e,$4c,$64,$59
	defb $9c,$cb,$68,$aa,$6c,$23,$54,$24,$dd,$2e,$ab,$e8,$0c,$74,$6b,$6a
	defb $f9,$0a,$ac,$22,$96,$6b,$67,$f9,$00,$45,$01,$47,$ff,$e9,$11,$75
	defb $48,$ad,$8c,$72,$a9,$6d,$f9,$12,$9b,$00,$30,$e8,$11,$3e,$00,$20
	defb $e8,$0d,$41,$ec,$12,$4c,$ec,$12,$93,$02,$73,$f1,$f8,$8f,$52,$4c
	defb $9a,$00,$47,$e8,$22,$8f,$ef,$0f,$55,$4c,$49,$d9,$44,$ef,$07,$53
	defb $01,$f1,$f0,$ef,$00,$93,$00,$4f,$f9,$0f,$d4,$00,$8f,$4e,$21,$e9
	defb $0c,$54,$43,$5d,$f9,$00,$5d,$00,$20,$e8,$11,$3e,$38,$95,$ea,$07
	defb $61,$e8,$02,$6c,$8a,$ae,$39,$b5,$ca,$71,$c9,$6f,$00,$1f,$e9,$08
	defb $70,$01,$fc,$f8,$aa,$66,$c8,$ba,$f9,$0a,$74,$23,$5a,$eb,$06,$76
	defb $2d,$c9,$8b,$6f,$2d,$9a,$00,$7e,$ec,$0e,$67,$e8,$09,$a8,$00,$f2
	defb $c9,$b3,$c8,$67,$f9,$01,$9c,$2d,$46,$8b,$66,$2d,$47,$8b,$f6,$e8
	defb $09,$a9,$22,$59,$6b,$6a,$39,$5d,$c2,$93,$e9,$1f,$3e,$01,$20,$1f
	defb $2f,$23,$a1,$34,$fa,$d9,$03,$60,$53,$02,$04,$f8,$00,$f9,$13,$3e
	defb $02,$07,$07,$03,$c3,$c5,$03,$ff,$ff,$aa,$55,$f6,$ed,$21,$28,$3e
	defb $02,$d4,$a8,$50,$e6,$0b,$fe,$f8,$08,$31,$00,$7e,$fa,$99,$41,$f8
	defb $04,$22,$05,$01,$07,$0f,$0c,$1c,$08,$61,$01,$a5,$21,$f3,$2d,$57
	defb $04,$c0,$f0,$7c,$1e,$1d,$fa,$0b,$3f,$80,$42,$04,$00,$46,$ff,$c3
	defb $81,$32,$62,$04,$c3,$ff,$0e,$00,$81,$71,$63,$f7,$93,$61,$00,$80
	defb $71,$75,$02,$00,$08,$18,$b0,$76,$98,$34,$02,$38,$44,$9a,$b8,$34
	defb $01,$63,$94,$b8,$34,$01,$18,$a5,$98,$34,$14,$01,$9e,$90,$4f,$10
	defb $28,$28,$28,$ee,$29,$fd,$06,$4f,$02,$05,$05,$05,$0d,$15,$b7,$ac
	defb $78,$34,$06,$e0,$10,$ef,$01,$08,$0e,$0d,$90,$7e,$fa,$13,$3f,$00
	defb $7e,$b1,$42,$d8,$33,$f7,$91,$81,$f8,$01,$51,$d0,$76,$04,$0f,$a2
	defb $a2,$9a,$44,$58,$00,$04,$0f,$14,$64,$84,$f3,$64,$32,$03,$84,$98
	defb $a0,$3d,$64,$32,$05,$90,$91,$90,$d0,$1e,$01,$46,$52,$0e,$e3,$12
	defb $12,$0f,$85,$7e,$00,$0f,$a8,$18,$a9,$a9,$1e,$14,$0f,$27,$f2,$06
	defb $f1,$01,$01,$0f,$30,$c0,$00,$fa,$14,$7e,$f1,$bb,$61,$09,$0c,$00
	defb $22,$36,$2a,$2a,$22,$22,$22,$0c,$20,$00,$04,$72,$8b,$fa,$82,$72
	defb $5a,$b8,$0d,$c8,$24,$23,$22,$2c,$0c,$00,$10,$00,$b1,$92,$11,$10
	defb $38,$5a,$b8,$04,$e8,$28,$e8,$29,$26,$5a,$b8,$04,$9c,$a2,$be,$a0
	defb $9c,$5a,$b8,$04,$78,$80,$70,$08,$f0,$fa,$23,$7e,$00,$00,$fa,$99
	defb $c1,$f8,$01,$22,$05,$18,$1c,$0c,$0f,$07,$01,$a6,$21,$52,$22,$fa
	defb $2e,$d7,$07,$1e,$1d,$3a,$fd,$fa,$d4,$a8,$41,$f8,$01,$3f,$d0,$21
	defb $00,$38,$44,$01,$f4,$f7,$e0,$fa,$01,$e1,$02,$e0,$e0,$c0,$60,$00
	defb $ff

data2:
	defb $01,$38,$ff,$20,$00,$1f,$db,$fe,$ec,$fc,$38,$e0,$c0,$8a,$00,$24
	defb $11,$02,$90,$38,$07,$07,$a2,$14,$99,$11,$42,$40,$38,$00,$04,$22
	defb $91,$d0,$89,$82,$64,$38,$1f,$04,$08,$c1,$f8,$3f,$7f,$6e,$fc,$38
	defb $00,$80,$ff,$13,$03,$e7,$fc,$1c,$38,$00,$40,$6f,$ff,$ff,$40,$a7
	defb $00,$38,$00,$08,$ff,$f9,$f0,$1f,$3c,$a7,$21,$38,$00,$4a,$a7,$10
	defb $10,$8b,$e3,$fb,$38,$00,$df,$ff,$7f,$ef,$83,$93,$27,$38,$00,$ff
	defb $ff,$f7,$e7,$cf,$df,$9f,$38,$7f,$40,$00,$20,$00,$78,$00,$e4,$0f
	defb $0b,$06,$ea,$f6,$ff,$a6,$fd,$ef,$f6,$38,$00,$04,$f6,$7f,$fb,$ff
	defb $77,$27,$0b,$1f,$fb,$db,$dd,$f8,$73,$e2,$c8,$38,$ff,$7b,$ec,$06
	defb $23,$66,$0b,$91,$38,$ff,$6d,$00,$64,$66,$20,$35,$22,$38,$f6,$36
	defb $8d,$8b,$66,$95,$0a,$9b,$2b,$38,$ff,$9b,$db,$7f,$d6,$ee,$7f,$6f
	defb $38,$00,$20,$0b,$00,$df,$26,$0b,$02,$fc,$f9,$fc,$40,$00,$03,$fe
	defb $38,$3f,$3f,$40,$00,$01,$87,$87,$f8,$06,$0c,$02,$fe,$ff,$fd,$b8
	defb $0c,$1f,$7c,$e0,$38,$bf,$ff,$fe,$f0,$84,$0a,$53,$50,$38,$e0,$f6
	defb $30,$f4,$5a,$32,$78,$bd,$38,$01,$4b,$82,$11,$93,$42,$24,$11,$38
	defb $3f,$7f,$1f,$ff,$3f,$02,$e0,$90,$04,$38,$f8,$30,$01,$80,$12,$22
	defb $00,$10,$38,$21,$0b,$8c,$30,$50,$82,$41,$00,$38,$12,$09,$00,$92
	defb $40,$0a,$24,$1f,$00,$38,$14,$01,$ab,$13,$06,$cd,$0d,$00,$38,$dc
	defb $f8,$f8,$f0,$e2,$c2,$c9,$00,$38,$08,$9a,$9c,$55,$63,$40,$48,$80
	defb $38,$24,$92,$01,$1f,$48,$92,$02,$50,$00,$38,$22,$48,$09,$40,$4c
	defb $1b,$59,$1f,$38,$1b,$7b,$18,$bd,$fc,$e6,$e1,$49,$38,$07,$37,$0f
	defb $8f,$7f,$1e,$3c,$00,$04,$38,$9f,$2f,$7f,$3f,$20,$1f,$02,$03,$38
	defb $f9,$40,$00,$02,$bf,$ff,$f8,$d8,$0c,$00,$00,$d8,$0c,$00,$1f,$f8
	defb $01,$0c,$1f,$fb,$df,$df,$fb,$fa,$ff,$ef,$f0,$38,$fe,$76,$f9,$f9
	defb $f1,$64,$00,$00,$38,$15,$49,$49,$24,$24,$92,$06,$00,$38,$49,$25
	defb $24,$a5,$92,$1f,$52,$4f,$c9,$38,$24,$29,$99,$96,$48,$49,$5a,$0d
	defb $38,$d3,$65,$09,$4d,$cd,$38,$65,$c0,$38,$f7,$bf,$be,$df,$bf,$f6
	defb $bf,$00,$38,$ef,$63,$16,$26,$2d,$02,$fc,$fc,$fd,$20,$00,$05,$fc
	defb $fc,$38,$6f,$0f,$c6,$a3,$2c,$60,$0b,$1f,$fe,$00,$38,$fd,$ff,$f8
	defb $f0,$86,$09,$21,$24,$38,$d2,$8b,$01,$48,$49,$00,$64,$24,$38,$2a
	defb $13,$57,$bb,$9f,$e4,$6f,$60,$38,$52,$6c,$1f,$48,$84,$d2,$92,$a4
	defb $00,$38,$81,$98,$85,$90,$48,$44,$92,$21,$38,$62,$10,$09,$84,$a4
	defb $49,$38,$3e,$38,$08,$44,$00,$dc,$fc,$fc,$dc,$08,$ec,$38,$08,$00
	defb $82,$10,$00,$02,$20,$27,$06,$03,$40,$08,$00,$02,$21,$42,$38,$05
	defb $23,$41,$65,$42,$0d,$00,$05,$01,$40,$09,$01,$80,$38,$b0,$09,$fc
	defb $fc,$cc,$b8,$26,$37,$07,$02,$00,$20,$04,$00,$10,$02,$00,$38,$46
	defb $04,$0d,$87,$03,$21,$01,$27,$24,$01,$64,$a9,$83,$0b,$07,$00,$80
	defb $00,$00,$09,$00,$00,$24,$38,$02,$07,$03,$07,$07,$27,$07,$07,$38
	defb $f8,$a0,$4b,$38,$44,$20,$4c,$00,$24,$26,$30,$00,$1f,$40,$4d,$01
	defb $9f,$1f,$67,$2d,$07,$fe,$fc,$f8,$f0,$f0,$38,$e0,$80,$40,$4c,$08
	defb $08,$01,$38,$00,$02,$10,$00,$80,$12,$26,$30,$1f,$20,$00,$00,$92
	defb $00,$00,$22,$00,$38,$11,$0d,$04,$03,$41,$01,$00,$20,$38,$6b,$4e
	defb $ad,$4d,$5c,$9f,$df,$5f,$38,$80,$00,$c0,$80,$84,$02,$c0,$c0,$c2
	defb $78,$4c,$24,$50,$00,$40,$58,$4d,$04,$0f,$0f,$0f,$4f,$07,$38,$37
	defb $02,$fc,$fe,$fc,$20,$57,$78,$4c,$03,$40,$09,$00,$80,$f8,$01,$4c
	defb $19,$49,$08,$44,$42,$24,$48,$48,$13,$38,$b2,$5a,$7d,$38,$98,$85
	defb $40,$34,$38,$60,$00,$c0,$c0,$00,$02,$80,$08,$38,$44,$1f,$04,$00
	defb $41,$08,$00,$41,$38,$10,$26,$20,$12,$12,$01,$08,$0c,$38,$7e,$3c
	defb $3e,$4e,$0d,$36,$8b,$15,$38,$f8,$ec,$ec,$fc,$ec,$e4,$fc,$03,$f8
	defb $38,$04,$20,$21,$5d,$22,$46,$10,$38,$81,$1f,$1e,$9f,$0f,$1f,$1f
	defb $80,$38,$20,$7f,$fe,$fe,$fe,$7e,$fc,$27,$51,$0d,$7f,$6f,$6b,$6b
	defb $43,$c9,$0f,$38,$fc,$f8,$fc,$ec,$fc,$f8,$26,$00,$42,$46,$43,$42
	defb $00,$38,$21,$42,$44,$50,$09,$49,$38,$77,$79,$3f,$3f,$19,$0d,$03
	defb $03,$38,$44,$25,$46,$45,$61,$09,$87,$07,$07,$47,$07,$07,$87,$07
	defb $38,$b8,$80,$4b,$02,$b8,$38,$44,$61,$51,$03,$90,$02,$38,$9f,$c1
	defb $4d,$04,$e0,$e0,$c0,$c0,$c4,$22,$54,$00,$38,$25,$6c,$09,$00,$90
	defb $01,$01,$49,$38,$22,$1f,$38,$7e,$84,$48,$07,$49,$e0,$90,$d8,$b8
	defb $bf,$ff,$38,$23,$61,$18,$00,$09,$80,$ff,$7f,$38,$7f,$7f,$33,$31
	defb $3d,$3e,$fe,$ff,$38,$c0,$c0,$c1,$00,$80,$c4,$c0,$c0,$38,$09,$44
	defb $51,$02,$80,$0a,$02,$38,$02,$05,$23,$03,$03,$43,$01,$09,$38,$65
	defb $03,$78,$f8,$78,$f0,$26,$4e,$25,$50,$23,$51,$04,$99,$08,$38,$49
	defb $00,$41,$46,$17,$20,$04,$38,$21,$58,$0c,$44,$69,$0c,$4c,$64,$38
	defb $02,$51,$98,$22,$30,$b8,$3c,$9c,$38,$00,$01,$20,$62,$5d,$38,$41
	defb $24,$4c,$1e,$20,$20,$42,$38,$01,$44,$00,$06,$81,$10,$01,$41,$38
	defb $26,$86,$53,$13,$0d,$82,$33,$01,$38,$fc,$ec,$f4,$fc,$fc,$74,$bc
	defb $d8,$38,$81,$61,$26,$20,$41,$55,$02,$10,$02,$80,$67,$81,$83,$42
	defb $21,$4a,$03,$27,$07,$0c,$80,$27,$64,$00,$f4,$20,$65,$01,$fc,$44
	defb $27,$04,$25,$41,$20,$61,$01,$02,$38,$24,$7d,$0a,$12,$90,$38,$10
	defb $0c,$38,$27,$03,$01,$41,$01,$22,$50,$38,$07,$41,$51,$26,$5c,$25
	defb $6a,$25,$4a,$26,$6a,$00,$d8,$20,$4b,$00,$f9,$45,$4e,$43,$58,$01
	defb $20,$02,$86,$55,$01,$1f,$3f,$22,$8d,$11,$38,$80,$80,$92,$80,$80
	defb $84,$80,$c0,$38,$01,$01,$41,$09,$01,$41,$03,$11,$38,$1c,$01,$ff
	defb $bd,$23,$2b,$03,$fd,$38,$fe,$b2,$60,$4c,$01,$04,$38,$25,$2c,$60
	defb $55,$38,$00,$80,$8d,$01,$38,$c1,$25,$54,$00,$c0,$45,$54,$08,$02
	defb $22,$02,$02,$43,$03,$13,$03,$38,$25,$52,$40,$89,$12,$12,$38,$e0
	defb $e1,$e0,$e0,$e2,$c0,$40,$42,$38,$08,$08,$08,$18,$18,$58,$18,$38
	defb $38,$87,$22,$6f,$17,$00,$04,$40,$38,$4c,$4c,$66,$67,$73,$74,$7f
	defb $7f,$38,$b8,$38,$70,$f0,$d0,$80,$a2,$e0,$38,$40,$04,$20,$42,$21
	defb $8f,$07,$38,$40,$70,$b0,$d0,$f0,$f9,$f8,$27,$04,$01,$08,$80,$44
	defb $4c,$12,$12,$38,$0b,$c9,$01,$92,$49,$01,$24,$24,$38,$ec,$ec,$f4
	defb $ec,$b4,$d4,$64,$0c,$78,$86,$02,$08,$00,$42,$67,$4c,$03,$8f,$1f
	defb $1b,$1f,$67,$2c,$02,$fd,$ee,$e0,$46,$2c,$00,$cf,$20,$00,$02,$0d
	defb $e9,$00,$38,$37,$07,$d8,$dc,$fc,$08,$80,$fc,$38,$10,$a0,$86,$02
	defb $38,$1c,$3e,$42,$8d,$01,$86,$02,$58,$78,$07,$12,$80,$80,$c2,$c0
	defb $38,$00,$21,$25,$99,$07,$00,$08,$41,$38,$03,$27,$07,$03,$41,$8a
	defb $04,$38,$f8,$f0,$f8,$e8,$20,$ab,$00,$f8,$38,$75,$44,$55,$26,$61
	defb $81,$8d,$03,$9f,$1f,$38,$89,$41,$94,$02,$e0,$e4,$f0,$38,$96,$21
	defb $42,$22,$51,$38,$00,$03,$f0,$c1,$1a,$12,$26,$47,$25,$96,$02,$68
	defb $00,$a2,$26,$91,$44,$67,$02,$00,$00,$44,$27,$4c,$06,$3f,$01,$10
	defb $1a,$04,$95,$12,$58,$74,$25,$97,$05,$40,$40,$38,$43,$03,$03,$42
	defb $76,$00,$03,$38,$8e,$24,$8e,$04,$80,$c9,$c0,$38,$40,$23,$72,$64
	defb $4c,$38,$b8,$00,$78,$23,$b8,$27,$4b,$03,$08,$41,$00,$08,$20,$b7
	defb $38,$73,$01,$7e,$7f,$43,$ba,$07,$38,$c0,$44,$40,$80,$84,$00,$01
	defb $27,$81,$00,$83,$82,$79,$38,$37,$20,$4c,$03,$01,$00,$90,$38,$61
	defb $b2,$00,$22,$26,$30,$0a,$01,$10,$12,$00,$12,$00,$49,$04,$38,$18
	defb $90,$44,$57,$01,$fc,$78,$38,$41,$24,$58,$24,$4c,$38,$54,$63,$4c
	defb $47,$bd,$22,$49,$22,$9e,$78,$4c,$25,$4c,$0a,$48,$38,$fc,$5c,$00
	defb $80,$54,$20,$dc,$fc,$38,$23,$a9,$44,$82,$0d,$02,$38,$37,$04,$80
	defb $1e,$06,$35,$1d,$0f,$38,$e0,$f2,$f8,$23,$85,$02,$fe,$df,$38,$63
	defb $86,$85,$83,$23,$6a,$23,$6a,$01,$38,$f0,$25,$ab,$00,$78,$20,$6b
	defb $02,$38,$90,$01,$40,$6f,$26,$66,$62,$8d,$45,$ad,$00,$f0,$23,$85
	defb $06,$fd,$f9,$f9,$f4,$38,$00,$48,$25,$bb,$05,$00,$84,$d0,$38,$80
	defb $10,$40,$72,$00,$04,$27,$58,$09,$90,$00,$02,$90,$00,$01,$8c,$38
	defb $00,$91,$25,$49,$23,$8e,$13,$38,$12,$1a,$12,$12,$92,$19,$05,$24
	defb $38,$40,$82,$80,$40,$44,$00,$40,$84,$38,$13,$25,$b5,$01,$03,$93
	defb $26,$68,$05,$80,$e1,$a0,$a0,$d2,$d0,$26,$4e,$07,$10,$00,$04,$41
	defb $01,$09,$01,$83,$f8,$01,$4b,$00,$40,$a0,$c9,$08,$38,$7f,$7e,$7e
	defb $7e,$7c,$7c,$7c,$78,$38,$50,$81,$69,$58,$42,$04,$1f,$1f,$1e,$9c
	defb $39,$38,$46,$06,$80,$ff,$f0,$8c,$9e,$39,$38,$25,$4c,$22,$8e,$26
	defb $99,$01,$00,$84,$21,$50,$03,$42,$00,$08,$38,$42,$17,$03,$f7,$3b
	defb $33,$d9,$58,$0a,$20,$00,$03,$7f,$ff,$38,$02,$81,$0b,$00,$f7,$58
	defb $0a,$04,$f9,$f9,$fc,$fe,$fe,$98,$e1,$04,$3f,$5f,$4f,$38,$f0,$c1
	defb $0b,$80,$e1,$26,$0b,$02,$3f,$ff,$ef,$24,$16,$26,$e1,$24,$37,$23
	defb $16,$03,$f6,$f4,$38,$01,$61,$0b,$03,$8f,$cf,$38,$07,$c1,$0b,$02
	defb $f8,$9f,$ff,$43,$16,$a7,$e3,$45,$0b,$00,$1f,$c1,$0b,$1f,$f6,$e3
	defb $e8,$ec,$cf,$db,$ff,$e6,$38,$6c,$4d,$49,$96,$a6,$90,$7d,$4a,$38
	defb $90,$ff,$80,$34,$cd,$82,$34,$ea,$38,$59,$23,$92,$d9,$0b,$01,$d7
	defb $5f,$27,$48,$1f,$7b,$44,$20,$ad,$03,$b0,$19,$38,$32,$89,$93,$52
	defb $24,$24,$94,$25,$38,$40,$36,$24,$42,$99,$92,$a4,$2a,$38,$83,$d8
	defb $08,$93,$48,$49,$0c,$aa,$51,$38,$f8,$1f,$01,$c8,$2a,$13,$90,$4c
	defb $38,$13,$23,$e4,$05,$1f,$4f,$8f,$a7,$38,$f8,$e1,$08,$e5,$00,$78
	defb $c1,$e5,$04,$00,$fb,$f7,$d3,$9b,$45,$eb,$1f,$39,$e7,$ff,$3f,$bf
	defb $ff,$3f,$7f,$38,$30,$f2,$f2,$e0,$cd,$c9,$c0,$ce,$38,$20,$1f,$cd
	defb $00,$33,$b2,$94,$4f,$38,$80,$52,$00,$24,$42,$1a,$10,$90,$8a,$38
	defb $d8,$4c,$7e,$be,$af,$f7,$ff,$5b,$38,$9f,$8f,$77,$fe,$7f,$7f,$ff
	defb $bf,$38,$ef,$df,$e7,$6f,$bf,$e5,$04,$0b,$12,$21,$36,$84,$c4,$ca
	defb $d1,$fd,$ff,$38,$b7,$f9,$ff,$bf,$1f,$47,$a7,$ff,$38,$fe,$21,$a3
	defb $02,$ff,$67,$b7,$6f,$05,$83,$0b,$00,$de,$20,$36,$05,$ef,$ff,$e7
	defb $f7,$38,$5f,$43,$36,$01,$ef,$ef,$e7,$02,$0b,$03,$fc,$ff,$fb,$fb
	defb $24,$1a,$47,$0b,$01,$bf,$bf,$e4,$03,$0b,$1f,$84,$38,$fd,$fd,$fe
	defb $ff,$9f,$80,$c0,$9a,$38,$e5,$fb,$19,$08,$e7,$63,$7f,$1c,$38,$05
	defb $37,$7f,$bf,$ff,$0f,$4f,$ed,$38,$fa,$fe,$d4,$14,$a1,$d4,$0a,$94
	defb $c9,$38,$45,$42,$b2,$12,$89,$64,$49,$25,$38,$22,$59,$49,$44,$24
	defb $94,$26,$9f,$0c,$45,$24,$29,$92,$92,$92,$a5,$92,$38,$24,$94,$4b
	defb $49,$21,$31,$12,$54,$38,$a4,$42,$32,$29,$14,$92,$a4,$b6,$38,$47
	defb $93,$53,$27,$97,$93,$97,$a7,$f8,$13,$0c,$05,$fe,$fc,$fd,$fb,$fb
	defb $f7,$6e,$09,$00,$df,$6a,$1c,$1f,$38,$9d,$99,$db,$9c,$dc,$f1,$90
	defb $f2,$38,$7c,$01,$42,$29,$21,$94,$92,$02,$38,$90,$44,$a2,$10,$24
	defb $a4,$40,$12,$38,$ff,$ef,$f6,$ff,$03,$b7,$ff,$fb,$f7,$38,$1c,$00
	defb $df,$25,$16,$ee,$03,$09,$e8,$29,$22,$00,$fb,$e6,$0e,$0b,$43,$36
	defb $2e,$01,$43,$1a,$64,$0b,$0e,$80,$da,$00,$24,$93,$80,$9a,$c5,$38
	defb $02,$c9,$24,$12,$42,$48,$2e,$12,$1f,$04,$a6,$25,$24,$4a,$93,$0b
	defb $24,$38,$9f,$9f,$5f,$cf,$4d,$b2,$db,$ff,$38,$e5,$f0,$f8,$ff,$df
	defb $96,$c6,$f6,$38,$c9,$f7,$ff,$9f,$83,$06,$53,$30,$9d,$38,$24,$24
	defb $e4,$42,$17,$1f,$d9,$38,$99,$90,$96,$c4,$29,$eb,$fe,$7f,$38,$92
	defb $96,$49,$a9,$16,$42,$ed,$cb,$38,$49,$49,$aa,$2d,$96,$d3,$58,$ed
	defb $38,$2f,$4f,$cf,$04,$b7,$9f,$df,$7f,$bf,$f8,$13,$0c,$1f,$df,$bf
	defb $bf,$bf,$73,$7f,$3f,$76,$38,$de,$df,$9e,$ff,$bd,$3e,$df,$7f,$38
	defb $64,$62,$65,$e4,$e4,$f6,$e4,$e4,$38,$b4,$00,$26,$90,$8f,$0e,$3f
	defb $7f,$fb,$38,$92,$a0,$14,$d2,$f0,$f8,$ea,$70,$38,$ff,$db,$4a,$08
	defb $46,$0b,$00,$f7,$21,$0a,$45,$17,$a1,$0b,$00,$7f,$f8,$08,$0c,$02
	defb $e7,$38,$ff,$60,$2b,$86,$0b,$03,$3f,$df,$bf,$7f,$e7,$04,$0b,$20
	defb $2b,$22,$37,$f9,$15,$22,$69,$08,$1b,$ff,$38,$e1,$e4,$e4,$f0,$f2
	defb $f2,$f9,$fc,$38,$22,$91,$da,$0b,$41,$64,$08,$4b,$38,$44,$29,$49
	defb $13,$ab,$4c,$40,$2d,$38,$0b,$1f,$2f,$36,$67,$7f,$bf,$07,$38,$b6
	defb $94,$5c,$dc,$ec,$79,$bc,$f9,$38,$49,$d2,$9a,$91,$9a,$4b,$91,$2a
	defb $38,$bb,$77,$c5,$6c,$2b,$25,$50,$1f,$9b,$38,$07,$c9,$fc,$bf,$41
	defb $f9,$3f,$03,$38,$d7,$f0,$3e,$3f,$87,$68,$6c,$6d,$38,$fd,$34,$0c
	defb $e5,$fe,$ff,$17,$ed,$38,$ef,$9f,$bf,$01,$bf,$df,$24,$2b,$78,$16
	defb $00,$df,$ed,$04,$09,$a9,$2c,$1b,$ff,$38,$6c,$7c,$66,$66,$72,$32
	defb $38,$ba,$38,$df,$fd,$7f,$67,$e5,$f3,$e3,$60,$38,$fb,$d9,$5b,$f7
	defb $b7,$1f,$1f,$cf,$99,$2b,$0b,$f8,$fc,$fe,$38,$f8,$f4,$f4,$f8,$f4
	defb $f8,$7c,$3a,$38,$00,$00,$cf,$24,$62,$01,$17,$3f,$f8,$01,$0c,$07
	defb $7f,$bb,$fb,$df,$87,$ef,$6f,$7f,$f8,$0b,$0c,$01,$fd,$fd,$cb,$59
	defb $c2,$ed,$29,$40,$64,$e2,$4c,$0b,$03,$df,$fd,$fd,$df,$f9,$15,$22
	defb $03,$de,$ff,$fb,$fe,$2e,$44,$1d,$fe,$fe,$de,$df,$f7,$bb,$ff,$67
	defb $38,$40,$54,$0a,$91,$24,$e4,$e4,$7f,$38,$44,$a4,$59,$14,$99,$9a
	defb $8f,$ff,$38,$97,$a7,$27,$23,$33,$11,$49,$ef,$38,$fc,$3e,$1f,$7f
	defb $63,$cb,$9f,$97,$38,$a6,$52,$19,$26,$46,$24,$26,$9f,$1f,$49,$48
	defb $26,$25,$90,$c8,$8e,$91,$38,$b5,$52,$09,$ac,$92,$42,$a9,$24,$38
	defb $ed,$24,$49,$cb,$24,$52,$4a,$92,$38,$fe,$db,$6b,$6d,$f4,$02,$66
	defb $52,$7b,$38,$39,$80,$0b,$f8,$13,$0c,$19,$9d,$df,$e3,$f2,$fe,$ff
	defb $fb,$ec,$38,$5c,$f6,$f9,$7e,$46,$fc,$f8,$7f,$38,$8e,$af,$5d,$4f
	defb $6d,$5d,$bb,$3b,$b8,$0c,$13,$7e,$df,$38,$98,$9c,$da,$f2,$e6,$fc
	defb $fa,$fe,$38,$37,$3f,$3b,$7f,$7b,$7d,$7e,$6d,$39,$5a,$ca,$65,$20
	defb $0b,$ec,$02,$41,$01,$4d,$00,$f8,$02,$0c,$4c,$40,$e5,$05,$0b,$00
	defb $f6,$2b,$28,$0b,$f7,$ff,$f3,$38,$fa,$7f,$de,$fc,$bb,$bc,$ff,$9f
	defb $98,$39,$20,$0b,$f8,$08,$0c,$0c,$fb,$38,$fb,$fc,$ef,$f7,$f4,$ff
	defb $db,$db,$38,$fb,$bf,$61,$18,$27,$0b,$81,$2b,$27,$2e,$28,$65,$01
	defb $fe,$fe,$8e,$3a,$25,$16,$26,$9a,$a9,$42,$1f,$ff,$38,$52,$2a,$25
	defb $51,$4a,$95,$24,$a4,$38,$62,$4d,$21,$63,$2b,$17,$c3,$9f,$38,$4a
	defb $52,$92,$2a,$24,$44,$4a,$24,$38,$52,$4d,$48,$0d,$42,$b3,$10,$8a
	defb $93,$38,$12,$b5,$29,$57,$23,$59,$49,$0b,$f9,$1c,$22,$1f,$fe,$eb
	defb $f9,$fd,$fe,$f3,$fd,$fe,$38,$8f,$f7,$3f,$33,$e3,$7b,$f9,$bc,$38
	defb $9f,$e3,$e7,$ed,$f0,$ee,$cc,$64,$38,$ef,$73,$f2,$bb,$bb,$06,$f7
	defb $af,$9f,$38,$fe,$6c,$05,$a3,$0b,$07,$7b,$6d,$ff,$f6,$6f,$7f,$eb
	defb $38,$61,$16,$21,$eb,$39,$41,$0a,$fa,$fc,$f8,$b1,$e7,$ff,$38,$fa
	defb $ff,$01,$92,$20,$90,$0b,$0e,$38,$4d,$60,$37,$bf,$f7,$ce,$fa,$57
	defb $38,$b2,$20,$e2,$01,$14,$6f,$26,$24,$10,$7d,$fe,$e5,$f3,$3c,$bc
	defb $87,$7b,$38,$9f,$97,$b0,$40,$0b,$a0,$24,$5a,$58,$00,$04,$1f,$1f
	defb $87,$47,$69,$f8,$05,$0c,$48,$07,$00,$38,$2b,$1b,$00,$f7,$6c,$4c
	defb $e8,$1b,$22,$05,$67,$fb,$ff,$8f,$e5,$f7,$e6,$03,$0b,$1f,$95,$92
	defb $c8,$cd,$e6,$e2,$db,$e7,$38,$2b,$c3,$b4,$29,$9b,$d9,$79,$6a,$38
	defb $20,$2d,$82,$21,$34,$04,$41,$6a,$38,$50,$08,$4c,$42,$21,$0b,$98
	defb $0a,$44,$38,$a3,$91,$99,$05,$67,$27,$4e,$9d,$78,$00,$2a,$0c,$ef
	defb $0c,$62,$c1,$0b,$00,$fd,$28,$bb,$48,$65,$0e,$38,$38,$fd,$fc,$3f
	defb $2f,$bf,$bf,$a7,$38,$04,$be,$b2,$60,$e6,$28,$4d,$08,$38,$93,$63
	defb $58,$0d,$81,$6a,$04,$90,$38,$00,$0d,$5f,$ae,$4c,$24,$c6,$bc,$38
	defb $de,$7f,$9b,$fd,$6f,$5e,$77,$2f,$68,$00,$de,$24,$2b,$1f,$f7,$ff
	defb $7b,$38,$e6,$e6,$fc,$d1,$b1,$b3,$8b,$ca,$38,$5b,$73,$93,$a7,$b4
	defb $30,$19,$4c,$38,$6d,$6d,$5b,$69,$1d,$94,$6c,$34,$38,$6e,$16,$c1
	defb $bb,$76,$02,$94,$89,$83,$38,$c8,$08,$65,$60,$40,$bd,$36,$42,$38
	defb $cb,$02,$24,$a4,$49,$10,$2e,$14,$07,$49,$48,$ac,$07,$61,$39,$09
	defb $60,$38,$11,$05,$f7,$3f,$e0,$80,$16,$20,$78,$00,$03,$37,$07,$83
	defb $68,$f9,$02,$42,$29,$3b,$41,$18,$f9,$25,$22,$00,$ed,$63,$e2,$2e
	defb $a2,$01,$d4,$eb,$24,$e3,$2a,$9b,$1a,$38,$10,$09,$a4,$48,$49,$22
	defb $26,$5f,$38,$a4,$11,$8b,$9f,$3e,$fd,$fb,$f2,$38,$fb,$f6,$96,$ef
	defb $6d,$3a,$9f,$eb,$59,$21,$00,$be,$64,$e1,$42,$e4,$2a,$cc,$ef,$0b
	defb $62,$04,$fb,$fd,$ff,$fe,$fb,$2b,$8f,$09,$38,$ef,$7f,$77,$db,$df
	defb $bf,$cf,$ef,$38,$4a,$4d,$2b,$4d,$07,$70,$38,$92,$02,$48,$41,$10
	defb $49,$26,$01,$12,$37,$23,$4f,$07,$43,$2f,$0d,$48,$38,$db,$ed,$df
	defb $ff,$a6,$ff,$f6,$a7,$38,$be,$20,$36,$00,$dd,$4d,$4c,$1f,$66,$66
	defb $bc,$a4,$ee,$f4,$0c,$8c,$38,$44,$06,$60,$50,$15,$83,$6d,$fd,$38
	defb $72,$9a,$30,$e1,$c8,$4b,$81,$b0,$38,$32,$41,$9f,$fa,$03,$1f,$34
	defb $20,$05,$38,$49,$7c,$ff,$03,$61,$98,$02,$24,$38,$5a,$8d,$f2,$dd
	defb $00,$24,$52,$83,$38,$5a,$12,$93,$d8,$99,$32,$7e,$e7,$38,$48,$05
	defb $09,$80,$d6,$c1,$60,$7c,$27,$e0,$0f,$95,$20,$12,$44,$21,$92,$08
	defb $38,$3f,$1f,$83,$43,$30,$04,$92,$48,$38,$00,$05,$6f,$df,$ff,$1f
	defb $3f,$37,$f9,$25,$22,$01,$f7,$f3,$4b,$ab,$2e,$09,$1f,$fe,$f7,$e7
	defb $cc,$c2,$d5,$99,$d9,$38,$ff,$d7,$af,$7f,$7e,$bc,$fd,$fb,$38,$ec
	defb $df,$93,$bb,$af,$df,$37,$b3,$38,$f9,$bf,$76,$ef,$7b,$0a,$6d,$af
	defb $fc,$38,$bc,$fc,$79,$e3,$e7,$0f,$3f,$67,$0b,$4c,$40,$ef,$08,$42
	defb $01,$f7,$e0,$79,$2c,$20,$65,$08,$fe,$38,$77,$db,$d8,$b6,$02,$40
	defb $59,$27,$b7,$0e,$11,$00,$4c,$21,$00,$94,$22,$38,$89,$02,$20,$54
	defb $03,$20,$88,$27,$db,$06,$48,$7c,$26,$40,$62,$10,$84,$38,$11,$1f
	defb $bd,$df,$ff,$de,$ef,$bf,$38,$6d,$fd,$bf,$ef,$fd,$fe,$df,$f7,$38
	defb $99,$de,$9f,$cf,$c6,$46,$c7,$e7,$38,$0a,$6d,$f7,$fb,$7d,$2c,$27
	defb $1f,$db,$38,$24,$a2,$e1,$34,$de,$01,$80,$ea,$38,$a4,$10,$02,$49
	defb $d0,$6d,$05,$5e,$38,$80,$56,$00,$25,$bf,$b4,$cd,$09,$38,$49,$4f
	defb $b6,$07,$bf,$b6,$d2,$ff,$3f,$38,$87,$2f,$2a,$10,$45,$17,$22,$17
	defb $13,$e7,$c7,$be,$fc,$07,$38,$21,$24,$04,$c2,$e1,$f8,$fb,$fc,$38
	defb $09,$90,$44,$24,$29,$45,$2c,$07,$9b,$19,$88,$c4,$08,$22,$e4,$f1
	defb $58,$00,$02,$4f,$00,$a0,$ae,$83,$04,$03,$10,$90,$26,$38,$81,$e1
	defb $26,$93,$e0,$12,$0c,$1e,$c7,$c7,$9f,$bf,$37,$6f,$7f,$3e,$38,$f5
	defb $ec,$df,$bd,$7d,$6f,$d6,$9a,$38,$fb,$b6,$db,$b6,$b4,$60,$cc,$ce
	defb $38,$60,$d7,$8f,$3f,$22,$00,$2f,$42,$22,$37,$02,$f7,$df,$7e,$e7
	defb $00,$0b,$ef,$02,$68,$03,$e0,$c6,$e0,$e4,$2d,$4d,$1f,$c8,$38,$7e
	defb $0c,$82,$22,$10,$09,$80,$48,$38,$44,$32,$78,$6e,$6f,$ca,$03,$20
	defb $38,$00,$c8,$09,$00,$54,$04,$22,$90,$38,$40,$92,$09,$04,$40,$89
	defb $82,$10,$42,$38,$7b,$0c,$0b,$01,$41,$29,$07,$43,$38,$fd,$bf,$df
	defb $fb,$ff,$6f,$26,$c8,$1f,$fe,$ff,$77,$f9,$df,$ff,$f8,$f9,$38,$f3
	defb $bf,$b7,$ed,$7f,$4f,$0f,$5f,$38,$9f,$9d,$67,$b3,$33,$b6,$ad,$a9
	defb $38,$f0,$fe,$ff,$37,$6d,$1d,$f4,$96,$33,$38,$a0,$00,$94,$9b,$88
	defb $b3,$66,$04,$38,$7f,$c0,$10,$f7,$b6,$4e,$96,$64,$38,$ca,$9f,$0f
	defb $f6,$26,$6e,$e6,$5c,$22,$70,$0e,$fe,$3e,$36,$e6,$62,$67,$38,$1f
	defb $7f,$3f,$7f,$e4,$02,$7f,$27,$79,$07,$07,$2f,$43,$f3,$4b,$38,$00
	defb $d6,$e8,$68,$07,$05,$38,$1a,$04,$c9,$48,$c8,$45,$0b,$05,$6a,$8a
	defb $c1,$7d,$1f,$07,$2e,$a2,$2a,$13,$02,$24,$96,$fa,$26,$0b,$07,$8f
	defb $43,$91,$91,$4b,$6b,$47,$e7,$38,$0b,$03,$ed,$db,$ff,$af,$86,$0b
	defb $60,$0b,$f9,$01,$07,$1f,$7d,$fd,$fa,$fa,$f5,$e8,$d8,$b2,$38,$8f
	defb $ac,$48,$4b,$93,$17,$9e,$7e,$38,$1c,$7c,$fc,$f8,$f8,$62,$70,$65
	defb $38,$7f,$ca,$6f,$fe,$cf,$05,$db,$fe,$b7,$38,$df,$db,$22,$00,$ad
	defb $81,$00,$fd,$40,$0b,$59,$0c,$83,$0b,$0b,$82,$92,$21,$08,$92,$91
	defb $80,$cc,$38,$13,$00,$48,$24,$bf,$09,$20,$12,$38,$10,$0a,$c0,$09
	defb $00,$49,$21,$27,$21,$05,$42,$08,$21,$12,$00,$25,$37,$1c,$18,$49
	defb $08,$21,$44,$38,$18,$80,$38,$33,$11,$20,$04,$90,$00,$48,$02,$38
	defb $76,$fe,$7e,$9c,$ff,$ba,$9d,$df,$38,$8b,$05,$f3,$e0,$d4,$ca,$81
	defb $92,$38,$6d,$0d,$1d,$9f,$5f,$7f,$3d,$1f,$38,$b5,$95,$eb,$ed,$c9
	defb $f4,$ee,$2f,$3d,$1f,$2c,$68,$30,$9c,$da,$83,$01,$38,$0f,$6e,$7c
	defb $70,$00,$0b,$77,$48,$38,$e5,$1c,$cc,$a3,$3c,$d0,$93,$1c,$38,$96
	defb $cc,$9c,$ce,$9c,$98,$1c,$ce,$ac,$38,$ff,$e2,$66,$e7,$c6,$ee,$ee
	defb $c6,$38,$23,$77,$66,$64,$36,$64,$64,$6c,$38,$e7,$f3,$13,$07,$4f
	defb $03,$97,$27,$78,$4e,$2a,$08,$67,$1a,$6a,$2b,$f8,$13,$0c,$22,$06
	defb $c3,$0b,$6b,$0c,$4f,$41,$1f,$fe,$fd,$fb,$ff,$ef,$ed,$38,$b0,$61
	defb $c4,$93,$93,$27,$0f,$3f,$38,$7c,$f9,$f9,$f3,$e2,$e6,$e3,$f7,$38
	defb $e1,$b3,$a1,$c3,$d3,$65,$bd,$0a,$bd,$38,$f7,$bb,$ae,$77,$7f,$97
	defb $fb,$bf,$38,$32,$20,$20,$00,$00,$7b,$4f,$40,$4c,$02,$01,$fb,$7f
	defb $59,$85,$1f,$fe,$6f,$fc,$fa,$a1,$38,$e2,$e0,$94,$82,$48,$24,$02
	defb $49,$38,$92,$81,$5a,$01,$42,$2b,$00,$91,$38,$94,$42,$e0,$69,$b0
	defb $f4,$b9,$d9,$1f,$38,$08,$c5,$01,$20,$34,$85,$01,$69,$38,$39,$30
	defb $32,$71,$f0,$e2,$e0,$70,$38,$20,$10,$02,$20,$24,$40,$64,$40,$38
	defb $eb,$59,$ff,$af,$1f,$93,$fe,$de,$ae,$38,$02,$92,$21,$05,$08,$4c
	defb $05,$42,$38,$9f,$4f,$4f,$cd,$9f,$9f,$9e,$9b,$38,$74,$eb,$da,$e8
	defb $d3,$da,$d8,$c1,$38,$1f,$fc,$5e,$01,$00,$fc,$4b,$00,$b0,$38,$29
	defb $b7,$80,$76,$33,$88,$a6,$57,$38,$d4,$a3,$39,$d6,$23,$32,$a4,$4f
	defb $38,$38,$db,$38,$59,$db,$1c,$31,$31,$bb,$38,$ce,$ee,$cc,$8e,$fe
	defb $8c,$8c,$dd,$38,$64,$44,$0c,$88,$cc,$0c,$08,$98,$38,$03,$c7,$27
	defb $13,$87,$87,$26,$37,$4a,$04,$fd,$bf,$ff,$fb,$df,$4e,$67,$88,$8e
	defb $b7,$6b,$ed,$0b,$89,$a7,$0b,$14,$f7,$b7,$dd,$38,$fd,$ef,$ff,$3f
	defb $ff,$fb,$bf,$be,$38,$fe,$ee,$d6,$6c,$bf,$df,$9f,$df,$58,$1f,$8b
	defb $aa,$07,$bd,$bf,$ff,$fd,$f7,$ee,$b7,$77,$58,$36,$0b,$b7,$7f,$dd
	defb $ec,$fe,$38,$d7,$f2,$f3,$90,$89,$a4,$26,$67,$63,$eb,$01,$fd,$f8
	defb $2f,$f3,$10,$f6,$fe,$f4,$d0,$a9,$04,$20,$38,$90,$89,$42,$80,$29
	defb $04,$80,$52,$38,$22,$5e,$1d,$84,$24,$12,$80,$49,$38,$49,$64,$92
	defb $d9,$c1,$ec,$44,$25,$38,$f8,$d1,$c2,$a4,$49,$c1,$c9,$99,$38,$84
	defb $41,$c9,$91,$83,$eb,$26,$0b,$1f,$fa,$bc,$84,$c1,$c9,$c0,$c4,$c8
	defb $38,$c8,$c4,$00,$29,$24,$00,$90,$12,$38,$d6,$f6,$3f,$d6,$ed,$3b
	defb $1d,$1b,$38,$4b,$13,$01,$2d,$12,$02,$82,$aa,$c2,$38,$b3,$1f,$3b
	defb $36,$3e,$6b,$69,$2d,$38,$ee,$b6,$93,$48,$4c,$23,$20,$b6,$38,$b6
	defb $46,$c0,$34,$0a,$a1,$4c,$48,$38,$48,$e4,$4d,$81,$34,$0e,$c1,$1f
	defb $21,$38,$61,$b0,$0f,$64,$a0,$1f,$13,$20,$38,$31,$31,$b7,$b1,$33
	defb $67,$3f,$77,$38,$88,$9c,$bf,$18,$98,$ff,$68,$f0,$38,$0c,$e8,$c8
	defb $09,$59,$c8,$98,$02,$41,$38,$8f,$87,$27,$1f,$31,$8a,$00,$07,$59
	defb $4c,$22,$09,$46,$1a,$28,$21,$e4,$02,$0b,$01,$fd,$6e,$78,$00,$03
	defb $fd,$fd,$5b,$db,$39,$87,$09,$b6,$bf,$f6,$ed,$6f,$fb,$38,$ff,$be
	defb $df,$20,$36,$0e,$be,$ef,$38,$fe,$fd,$7f,$be,$fd,$ff,$de,$fe,$38
	defb $df,$9f,$bf,$23,$ba,$4e,$ba,$05,$ff,$fd,$fb,$fd,$f7,$f6,$2f,$05
	defb $02,$bf,$ff,$ad,$52,$6b,$38,$65,$00,$b9,$2b,$dd,$08,$e4,$e0,$38
	defb $48,$00,$24,$44,$00,$49,$26,$a1,$19,$e1,$c0,$89,$80,$24,$12,$40
	defb $04,$38,$24,$02,$20,$12,$82,$20,$09,$80,$38,$08,$41,$08,$40,$12
	defb $00,$24,$41,$38,$78,$17,$91,$09,$41,$22,$0b,$01,$38,$63,$13,$2b
	defb $49,$15,$20,$08,$89,$38,$21,$85,$9b,$a7,$a7,$d3,$6f,$4f,$7a,$31
	defb $42,$37,$07,$38,$e2,$e1,$c0,$e9,$e4,$e0,$f2,$47,$9b,$1f,$20,$02
	defb $90,$04,$40,$48,$38,$1e,$36,$36,$37,$3b,$36,$67,$7b,$38,$c8,$f2
	defb $d0,$4c,$e6,$61,$70,$68,$38,$4a,$d4,$9a,$cb,$ff,$7f,$96,$05,$c5
	defb $38,$92,$c9,$92,$77,$29,$6c,$04,$f7,$38,$a6,$30,$04,$21,$0a,$18
	defb $ef,$7d,$38,$94,$82,$6a,$ff,$ff,$6e,$f7,$3a,$38,$08,$47,$f7,$ff
	defb $fd,$df,$ff,$40,$38,$63,$63,$66,$e6,$64,$2c,$0e,$3c,$0c,$5c,$30
	defb $30,$f2,$f8,$01,$38,$58,$00,$84,$22,$00,$48,$36,$78,$08,$8f,$4f
	defb $0f,$4d,$8f,$0f,$9d,$0f,$38,$4a,$90,$01,$bf,$7d,$26,$73,$25,$17
	defb $1e,$eb,$db,$7f,$b6,$6d,$38,$f7,$f6,$dd,$7f,$eb,$7f,$db,$fb,$38
	defb $ef,$76,$b7,$7e,$6f,$ff,$77,$fb,$38,$dd,$fd,$ed,$f6,$7e,$b6,$fb
	defb $77,$51,$83,$e1,$05,$fe,$fe,$ef,$fe,$ff,$dd,$96,$6b,$b3,$51,$02
	defb $d6,$f7,$b7,$34,$6b,$47,$0b,$11,$fd,$7f,$ef,$fa,$78,$e4,$38,$64
	defb $c1,$c8,$92,$00,$48,$42,$81,$38,$94,$00,$54,$7f,$11,$00,$22,$38
	defb $90,$82,$11,$88,$40,$04,$42,$21,$38,$52,$00,$12,$88,$00,$56,$36
	defb $b6,$05,$08,$10,$82,$11,$20,$44,$26,$99,$30,$05,$1f,$48,$11,$80
	defb $89,$44,$38,$c0,$14,$49,$81,$10,$0a,$20,$40,$38,$ef,$e7,$b7,$67
	defb $4d,$5f,$7f,$1f,$38,$f7,$ff,$6f,$ef,$ff,$9f,$87,$03,$1f,$38,$f2
	defb $f0,$f0,$f2,$f0,$f8,$f9,$f0,$38,$09,$40,$88,$04,$40,$20,$12,$00
	defb $38,$d9,$cf,$f6,$ff,$df,$f7,$bd,$ef,$38,$b8,$fc,$ef,$fd,$01,$7d
	defb $b7,$26,$1a,$08,$f8,$00,$ef,$fd,$24,$ff,$55,$bb,$38,$38,$e2,$04
	defb $9b,$d3,$fe,$2d,$ed,$38,$50,$0e,$ff,$6d,$66,$f7,$9a,$b8,$38,$00
	defb $97,$ff,$60,$9f,$06,$21,$0e,$58,$0a,$04,$24,$fd,$db,$4e,$f4,$58
	defb $0a,$1f,$92,$e8,$49,$00,$84,$38,$10,$f9,$f0,$00,$15,$20,$09,$9e
	defb $38,$02,$10,$21,$04,$90,$02,$11,$48,$38,$5e,$1d,$3d,$3e,$36,$3f
	defb $7b,$7d,$3a,$bd,$15,$b3,$f3,$ff,$af,$fc,$64,$38,$de,$f6,$ed,$cd
	defb $87,$02,$53,$03,$38,$de,$cd,$b7,$bb,$bf,$77,$36,$8f,$04,$fd,$ff
	defb $ee,$ef,$fb,$31,$20,$04,$38,$be,$dd,$f7,$fb,$20,$90,$47,$15,$00
	defb $db,$21,$16,$00,$fe,$4f,$fa,$04,$ff,$f7,$fe,$ba,$c0,$47,$91,$05
	defb $db,$ec,$c0,$09,$42,$90,$39,$5d,$13,$a4,$00,$91,$08,$02,$50,$38
	defb $80,$24,$08,$80,$12,$09,$40,$20,$38,$10,$89,$41,$00,$50,$98,$00
	defb $38,$31,$5f,$01,$11,$84,$22,$b2,$38,$40,$14,$02,$20,$10,$8a,$02
	defb $20,$38,$92,$00,$95,$80,$49,$12,$45,$6f,$38,$24,$05,$41,$35,$1f
	defb $45,$16,$02,$82,$30,$24,$71,$39,$06,$38,$0a,$00,$a0,$14,$02,$f3
	defb $26,$0b,$1f,$4e,$41,$88,$94,$44,$c2,$c9,$c9,$38,$c1,$d8,$00,$d2
	defb $2a,$44,$91,$b2,$38,$f9,$00,$04,$48,$40,$4b,$60,$61,$38,$20,$08
	defb $84,$90,$00,$1f,$24,$26,$06,$38,$fe,$b7,$de,$fb,$9f,$9b,$dd,$8f
	defb $38,$de,$f7,$fb,$7e,$d6,$bf,$fa,$6c,$38,$be,$4a,$f2,$d0,$82,$00
	defb $29,$00,$38,$db,$04,$01,$24,$48,$01,$02,$26,$79,$05,$7d,$48,$48
	defb $1e,$03,$23,$26,$58,$08,$03,$92,$16,$9e,$57,$bb,$ee,$c4,$38,$24
	defb $99,$22,$72,$26,$41,$32,$9f,$1c,$02,$45,$09,$1b,$1c,$38,$43,$49
	defb $90,$96,$0c,$a7,$25,$9a,$38,$00,$11,$80,$c9,$00,$84,$c3,$c8,$38
	defb $6e,$c8,$00,$02,$48,$23,$bf,$03,$38,$80,$02,$24,$35,$3f,$05,$40
	defb $29,$38,$80,$29,$01,$34,$3f,$36,$3f,$0e,$9f,$f7,$ff,$ba,$d4,$84
	defb $01,$68,$38,$7f,$ef,$f8,$49,$80,$12,$26,$01,$01,$fd,$d2,$31,$96
	defb $03,$01,$28,$44,$38,$31,$d4,$03,$92,$12,$00,$92,$57,$b6,$04,$24
	defb $10,$42,$12,$9b,$37,$2a,$0d,$48,$04,$91,$40,$48,$21,$04,$38,$84
	defb $02,$b0,$01,$04,$80,$26,$22,$02,$09,$40,$04,$2c,$fd,$09,$02,$80
	defb $38,$80,$12,$01,$20,$12,$81,$48,$27,$31,$04,$21,$04,$20,$04,$48
	defb $36,$3c,$03,$24,$02,$21,$2d,$21,$8a,$00,$03,$f8,$1c,$0b,$1d,$c4
	defb $e7,$e0,$c8,$f6,$e2,$ec,$f5,$38,$96,$84,$d3,$89,$94,$c3,$93,$19
	defb $38,$64,$c0,$68,$b4,$82,$30,$0a,$82,$38,$e0,$28,$04,$22,$b2,$01
	defb $48,$da,$ff
endScreen:

end asm
end sub

sub click()
asm
;Taken from
;https://zxspectrumcoding.wordpress.com/2019/02/02/sound-effects-in-your-game/
;BeepFX player by Shiru
;You are free to do whatever you want with this code

playBasic:
	ld a,0
play:
	ld hl,sfxData	;address of sound effects data

	di
	push ix
	push iy

	ld b,0
	ld c,a
	add hl,bc
	add hl,bc
	ld e,(hl)
	inc hl
	ld d,(hl)
	push de
	pop ix			;put it into ix

	ld a,(23624)	;get border color from BASIC vars to keep it unchanged
	rra
	rra
	rra
	and 7
	ld (sfxRoutineToneBorder  +1),a
	ld (sfxRoutineNoiseBorder +1),a
	ld (sfxRoutineSampleBorder+1),a


readData:
	ld a,(ix+0)		;read block type
	ld c,(ix+1)		;read duration 1
	ld b,(ix+2)
	ld e,(ix+3)		;read duration 2
	ld d,(ix+4)
	push de
	pop iy

	dec a
	jr z,sfxRoutineTone
	dec a
	jr z,sfxRoutineNoise
	dec a
	jr z,sfxRoutineSample
	pop iy
	pop ix
	ei
	pop bc
	ret
	

	

;play sample

sfxRoutineSample:
	ex de,hl
sfxRS0:
	ld e,8
	ld d,(hl)
	inc hl
sfxRS1:
	ld a,(ix+5)
sfxRS2:
	dec a
	jr nz,sfxRS2
	rl d
	sbc a,a
	and 16
sfxRoutineSampleBorder:
	or 0
	out (254),a
	dec e
	jr nz,sfxRS1
	dec bc
	ld a,b
	or c
	jr nz,sfxRS0

	ld c,6
	
nextData:
	add ix,bc		;skip to the next block
	jr readData



;generate tone with many parameters

sfxRoutineTone:
	ld e,(ix+5)		;freq
	ld d,(ix+6)
	ld a,(ix+9)		;duty
	ld (sfxRoutineToneDuty+1),a
	ld hl,0

sfxRT0:
	push bc
	push iy
	pop bc
sfxRT1:
	add hl,de
	ld a,h
sfxRoutineToneDuty:
	cp 0
	sbc a,a
	and 16
sfxRoutineToneBorder:
	or 0
	out (254),a

	dec bc
	ld a,b
	or c
	jr nz,sfxRT1

	ld a,(sfxRoutineToneDuty+1)	 ;duty change
	add a,(ix+10)
	ld (sfxRoutineToneDuty+1),a

	ld c,(ix+7)		;slide
	ld b,(ix+8)
	ex de,hl
	add hl,bc
	ex de,hl

	pop bc
	dec bc
	ld a,b
	or c
	jr nz,sfxRT0

	ld c,11
	jr nextData



;generate noise with two parameters

sfxRoutineNoise:
	ld e,(ix+5)		;pitch

	ld d,1
	ld h,d
	ld l,d
sfxRN0:
	push bc
	push iy
	pop bc
sfxRN1:
	ld a,(hl)
	and 16
sfxRoutineNoiseBorder:
	or 0
	out (254),a
	dec d
	jr nz,sfxRN2
	ld d,e
	inc hl
	ld a,h
	and 31
	ld h,a
sfxRN2:
	dec bc
	ld a,b
	or c
	jr nz,sfxRN1

	ld a,e
	add a,(ix+6)	;slide
	ld e,a

	pop bc
	dec bc
	ld a,b
	or c
	jr nz,sfxRN0

	ld c,7
	jr nextData


sfxData:

SoundEffectsData:
	defw SoundEffect0Data

SoundEffect0Data:
	defb 3 ;sample
	defw 900
	defw Sample0Data+0
	defb 1
	defb 0

Sample0Data:
	defb 255,255,255,255,255,255,255,255,255,255,254,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,247,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,253,255,255
	defb 255,255,255,231,255,251,255,255,255,255,255,254,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,223,241,244,255
	defb 127,255,249,251,115,255,119,255,201,254,79,255,143,248,127,249
	defb 255,223,247,63,7,250,30,7,196,127,15,15,192,127,192,63
	defb 252,31,254,31,252,79,255,224,127,124,63,243,124,121,248,204
	defb 249,143,195,129,0,140,64,4,65,48,1,66,67,100,58,15
	defb 144,31,207,240,12,239,255,255,255,252,95,255,255,255,255,255
	defb 255,255,255,247,255,255,255,255,127,255,127,255,255,248,239,226
	defb 127,0,248,24,64,199,242,251,185,255,207,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,253,255,191,223,255,251,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,253,159,254,3,255,248,31,255,31,255,255
	defb 248,255,241,255,254,255,248,31,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	defb 255,255,255,255
end asm
end sub

sub buscaCharset()
for n=30000 to 65000
if peek(n)=2 
	if peek(n+1)=1 
		if peek(n+2)=3 
			if peek(n+3)=8 
				charset=n+4
				n=65000
			end if
		end if
	end if 	
end if
next n
end sub