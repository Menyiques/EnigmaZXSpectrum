#include <print42.bas>
#include <zx0.bas>
#include <clearbox.bas>

declare function siguiente(pos as ubyte) as ubyte
declare function anterior(pos as ubyte) as ubyte    

dim a, tecla as Ubyte
dim n as Uinteger
dim idioma as ubyte
dim dir as Uinteger
dim pascua as ubyte=0
dim doublestep as ubyte =0
dim notch(2,1) as ubyte 'rotor, notch
dim off as string
dim reflector as ubyte=0 ' default reflectpor is B

declare Function codifica(ByVal c As ubyte) As ubyte

REM Rotor,RingStellum,CurrentPos
DIM rotorSetup(2,2) as Ubyte = {{0,0,0},{1,0,0},{2,0,0}}
DIM plugBoard(19) as Ubyte={99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99}
DIM startingPos(2) as Ubyte

REM 8 rotors+reflector, 26 wires + notch. If Notch is 99=>its a double Notch (M and Z)' Reflector - double notch
DIM rotorDefinition(9,26) as Ubyte = { _
       {24,16,18,4 ,12,13,5 ,22,7 ,14,3 ,21,2 ,23,24,19,14,10,13,6 ,8 ,1 ,25,12,2 ,20,0 }, _ ' Reflector B
       {5 ,20,13,6 ,4 ,21,8 ,17,22,20,7 ,14,11,9 ,18,13,3 ,19,2 ,23,24,6 ,17,15,9 ,12,0 }, _ ' Reflector C
       {4 ,9 ,10,2 ,7 ,1 ,23,9 ,13,16,3 ,8 ,2 ,9 ,10,18,7 ,3 ,0 ,22,6 ,13,5 ,20,4 ,10,16}, _ ' Rotor I
       {0 ,8 ,1 ,7 ,14,3 ,11,13,15,18,1 ,22,10,6 ,24,13,0 ,15,7 ,20,21,3 ,9 ,24,16,5 ,4 }, _ ' Rotor II
       {1 ,2 ,3 ,4 ,5 ,6 ,22,8 ,9 ,10,13,10,13,0 ,10,15,18,5 ,14,7 ,16,17,24,21,18,15,21}, _ ' Rotor III
       {4 ,17,12,18,11,20,3 ,19,16,7 ,10,23,5 ,20,9 ,22,23,14,1 ,13,16,8 , 6,15,24,2 ,9 }, _ ' Rotor IV
       {21,24,25,14,2 ,3 ,13,17,12,6 , 8,18,1 ,20,23,8 ,10,5 ,20,16,22,19,9 ,7 ,4 ,11,25}, _ ' Rotor V
       {9 ,14,4 ,18,10,15,6 ,24,16,7 ,17,19,1 ,20,11,2 ,13,19,8 ,25,3 ,16,12,5 ,21,23,99}, _ ' Rotor VI - double notch
       {13,24,7 ,4 ,2 ,12,22,16,4 ,15,8 ,11,15,1 ,6 ,16,10,17,3 ,18,21,9 ,14,19,5 ,20,99}, _ ' Rotor VII - double notch
       {5 ,9 ,14,4 ,15,6 ,17,7 ,20,18,25,7 ,3 ,16,11,2 ,10,21,12,3 ,19,13,24,1 ,8 ,22,99} _ ' Rotor VIII
       } 

DIM inverseRotorDefinition(9,25) as Ubyte={ _
       {0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 }, _ 
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

DIM memory(250,2) as ubyte
DIM memoryPointer as integer

DIM charset as uinteger

charset=@datos
inicializaPantalla()

REM Bucle principal
bb$=""
idioma=0
calculaNotches()

do
aa$=inkey$
if aa$<>bb$ AND code(aa$)>0
    tecla=code (aa$)

    if tecla=12 and memoryPointer>0 'UNDO
        moveRotor(2,1,0)
        memoryPointer=memoryPointer-1
        click()
    end if
    if (tecla=49) '1
        swapReflector()
    end if

    if (tecla=50 OR tecla=51 OR tecla=52) ' 2,3 o 4 Seleccionar rotor
        pintaRingSet(0)
        changeRotor(tecla-50)
        saveStartingPos()
        click()
    end if
    
    if (tecla=53 OR tecla=54 OR tecla=55) ' 5,6,7 rotar rotores forward
        pintaRingSet(0)    
        moveRotor(tecla-53,0,1)
        saveStartingPos()
        click()
    end if
    
    if (tecla=8)   'MAYS+5 Rotar rotor 0 backwards
        pintaRingSet(0)
        moveRotor(0,1,1)
        saveStartingPos()
        click()
    end if
    
    if (tecla=10) 'MAYS+6 Rotar rotor 1 backwards
        pintaRingSet(0)
        moveRotor(1,1,1)
        saveStartingPos()
        click()
    end if
    
    if (tecla=11) ' MAYS+7 Rotar rotor 2 backwards
        pintaRingSet(0)
        moveRotor(2,1,1)
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

        if memoryPointer>249
          startingPos(0)=rotorSetup(0,2)
          startingPos(1)=rotorSetup(1,2)
          startingPos(2)=rotorSetup(2,2)
          memoryPointer=0
        end if
        
        pintaRingSet(0)
        moveRotor(2,0,0)
        encoded=codifica(tecla-97)
        ilumina(encoded,1)
        click()
        
        do
        loop while tecla=code(inkey$)
        
        ilumina(encoded,0)
        memory(memoryPointer,0)=tecla-97
        memory(memoryPointer,1)=encoded
        memoryPointer=memoryPointer+1
        
    end if

    if tecla=6 
        memoryPointer=0
        rotorSetup(0,1)=rotorSetup(0,1)+1
        if rotorSetup(0,1)>25 
            rotorSetup(0,1)=0
        end if
        pintaRingSet(1)
        beep 0.001,-5
        pausa(10)
    end if
    if tecla=4 
        saveStartingPos()
        rotorSetup(1,1)=rotorSetup(1,1)+1
        if rotorSetup(1,1)>25 
            rotorSetup(1,1)=0
        end if
        pintaRingSet(1)
        beep 0.001,-5
        pausa(10)

    end if
    if tecla=5
        saveStartingPos() 
        rotorSetup(2,1)=rotorSetup(2,1)+1
        if rotorSetup(2,1)>25 
            rotorSetup(2,1)=0
        end if
        pintaRingSet(1)
        beep 0.001,-5
        pausa(10)
    end if

    if tecla=13
        border 0
        mainScreen(1)
        click()
        pausa(10)
        paper 0
        ink 0
        clearBox(0,0,32,8)
        clearBox(0,22,32,2)
        ink 7
        
        do
        if idioma=0
            dir=charset+1224
        else
            dir=charset+1352
        end if
        poke uinteger 23675,dir

        if idioma=0
            printat42 (0,0) :print42 "      EMULADOR MAQUINA ENIGMA       E=Eng "
            printat42 (1,0) :print42 "      -----------------------             ":print at 1,27;"\A\B\C\D"
            printat42 (2,0) :print42 "1,2,3,4 para elegir tipo rotor/ref        ":print at 2,27;"\E\F\G\H"
            printat42 (3,0) :print42 "CS+2,3,4 para ajustar Ringstellung        ":print at 3,27;"\I\J\K\L"
            printat42 (4,0) :print42 "6,7,8 o CS+6,7,8 para rotar rotores       ":print at 4,27;"\M\N\O\P"
            printat42 (5,0) :print42 "CS+[A..Z] para enchufar cables del panel  "
            printat42 (6,0) :print42 "[a..z] Para (des)codificar y CS+0 deshacer"
            printat42 (7,0) :print42 "SPACE para ver el historial               "
            printat42 (22,0):print42 " (c)2021-25 Menyiques Soft.   X=@setaseta " 
            printat42 (23,0):print42 " Tests por @desUBIKado(DEP) y @stropdasje "
        
        else
            printat42 (0,0) :print42 "      ENIGMA MACHINE EMULATOR       E=Esp "
            printat42 (1,0) :print42 "      -----------------------             ":print at 1,27;"\A\B\C\D"
            printat42 (2,0) :print42 "To select rotors: press 1,2,3,4           ":print at 2,27;"\E\F\G\H"
            printat42 (3,0) :print42 "To set Ringstellung: press CS+2,3,4       ":print at 3,27;"\I\J\K\L"
            printat42 (4,0) :print42 "To set Plugboard: press CS+[A..Z]         ":print at 4,27;"\M\N\O\P"
            printat42 (5,0) :print42 "To rotate rotors: press 5,6,7 or CS+5,6,7 "
            printat42 (6,0) :print42 "To encode/decode: press [a..z] & CS+0:UNDO"
            printat42 (7,0) :print42 "To see your encoding records: press SPACE "
            printat42 (22,0):print42 "  (c)2021-25 Menyiques Soft.  X=@setaseta " 
            printat42 (23,0):print42 "  Tests by @desUBIKado(RIP) & @stropdasje "
        end if
        
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
                pausa(250)
                do
                loop while inkey$=""
                    inicializaPantalla()
                pascua=1
            end if
        end if    
    end if
end if
bb$=aa$

if memoryPointer=250
    border 0
elseif memoryPointer>= 245 
    border 2
elseif memoryPointer>=240
    border 6
else
    border 7
end if

loop

sub saveStartingPos()
    startingPos(0)=rotorSetup(0,2)
    startingPos(1)=rotorSetup(1,2)
    startingPos(2)=rotorSetup(2,2)
    memoryPointer=0
end sub
sub swapReflector()
    reflector = (reflector+1) mod 2
    saveStartingPos()
    printReflector()
    click()
end sub
sub printReflector()

    letra(2,4,reflector+1,2)    
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
    pintaRotor(0,0)
    pintaRotor(1,0)
    pintaRotor(2,0)
    printReflector()
end sub

sub ilumina (byval letra as ubyte,byval modo as ubyte)
    dim filAtt1,filAtt2 as ubyte

    if modo=0
        filAtt1=71
        filAtt2=7
    else
        filAtt1=70
        filAtt2=70
    end if

    dir=22528

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
        print at 7,(n+1)*5;paper 1;bright 0;"  ";
    else
        aa$=str(rotorSetup(n,1)+1)
        if len(aa$)=1
            aa$=aa$+" "
        end if
    ink 7:paper 1:bright 0
    if n=0 then printat42(7,7)
    if n=1 then printat42(7,14)
    if n=2 then printat42(7,20)
    
    
    print42(aa$)
    end if
next n

end sub

sub pausa(byval tiempo as uinteger)
for n=0 to tiempo
    asm
     halt
    end asm
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
    calculaNotches()
end sub

sub moveRotor(rotor as ubyte,move as ubyte, free as ubyte)

          if rotor>0 and free=0       
                border 7    
           if (rotor=2 and move=0 and(notch(1,0)=rotorSetup(1,2) or notch(1,1)=rotorSetup(1,2))) or _
              (rotor=2 and move=1 and (notch(1,0)=anterior(rotorSetup(1,2)) or notch(1,1)=anterior(rotorSetup(1,2)) ))
                moveRotor(1,move,1)
                moveRotor(0,move,1)
           end if
           
           if (move=0 and (notch(rotor,0)=rotorSetup(rotor,2) OR notch(rotor,1)=rotorSetup(rotor,2))) or _
              (move=1 and (notch(rotor,0)=anterior(rotorSetup(rotor,2)) OR notch(rotor,1)=anterior(rotorSetup(rotor,2)) ))
                moveRotor(rotor-1,move,0)
           end if
           
         end if
        
        if move=0 'Mueve hacia delante          
          letra(5+rotor*5,4,rotorSetup(rotor,2),1) ' media letra
          pintaRotor(rotor,1)          
          rotorSetup(rotor,2)=siguiente(rotorSetup(rotor,2))
          letra(5+rotor*5,4,rotorSetup(rotor,2),0) ' letra entera
          pintaRotor(rotor,0)
        end if



        if move=1 'Mueve hacia atras
          letra(5+rotor*5,4,rotorSetup(rotor,2),1) ' media letra
          pintaRotor(rotor,1)
          rotorSetup(rotor,2)=anterior(rotorSetup(rotor,2))
          letra(5+rotor*5,4,rotorSetup(rotor,2),0) ' letra entera
          pintaRotor(rotor,0)
        end if
        
end sub

sub calculaNotches()
    for n=0 to 2
        if rotorSetup(n,0)>4 ' Si el rotor es 5 o superior tiene 2 notches
            notch(n,0)=12
            notch(n,1)=25
          else
            notch(n,0)=rotorDefinition(rotorSetup(n,0)+2,26) ' si no, tiene 1 notch que está definido en rotorSetup
            notch(n,1)=notch(n,0)
        end if     
    next n
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

function siguiente(pos as ubyte) as ubyte
    pos=pos+1
    if pos=26 then pos=0
    return pos
end function

function anterior(pos as ubyte) as ubyte
    pos=pos-1
    if pos=255 then pos=25
    return pos
end function

sub letra (byval x as ubyte,y as ubyte, letra as ubyte, byval media as ubyte)
    dim dir as uinteger
    dir=32*cast(integer,letra)+charset+96+40
    poke uinteger 23675,dir
    over 0:paper 7:ink 0:bright 1
    if media=0 'Para las letras enteras
        print at y,x;"\A\B";
        bright 0
        print at y+1,x;"\C\D";
    end if 
    if media=1 'Para las medias letras
        print at y,x;"\C\D";
        bright 0
        print at y+1,x;"\E\F";
        end if
    if media=2 'Para los numeros romanos
        paper 1
        ink 2
        bright 0
        print at y,x;"\A\B";
        print at y+1,x;"\C\D";
        
    end if 
    over 0
end sub

sub pintaRotor(byval pos as ubyte, byval tipo as ubyte)
    dir=charset
    poke uinteger 23675,dir
    dim x as ubyte
    x=pos*5+3+5
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



sub mainScreen(screen as ubyte)
    paper 0:ink 0:cls
    if screen=0
        dzx0Turbo(@screen0, 16384)
    else
        dzx0Turbo(@screen1, 16384)
    end if    
    
goto finnn
screen0:
    asm
        incbin "enigma.scr_.zx0"
    end asm
screen1:
    asm
        incbin "enigma2.scr_.zx0"
    end asm
finnn:
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
    ld hl,sfxData    ;address of sound effects data

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
    pop ix            ;put it into ix

    ld a,(23624)    ;get border color from BASIC vars to keep it unchanged
    rra
    rra
    rra
    and 7
    ld (sfxRoutineToneBorder  +1),a
    ld (sfxRoutineNoiseBorder +1),a
    ld (sfxRoutineSampleBorder+1),a


readData:
    ld a,(ix+0)        ;read block type
    ld c,(ix+1)        ;read duration 1
    ld b,(ix+2)
    ld e,(ix+3)        ;read duration 2
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
    add ix,bc        ;skip to the next block
    jr readData



;generate tone with many parameters

sfxRoutineTone:
    ld e,(ix+5)        ;freq
    ld d,(ix+6)
    ld a,(ix+9)        ;duty
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

    ld a,(sfxRoutineToneDuty+1)     ;duty change
    add a,(ix+10)
    ld (sfxRoutineToneDuty+1),a

    ld c,(ix+7)        ;slide
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
    ld e,(ix+5)        ;pitch

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
    add a,(ix+6)    ;slide
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

Function codifica(ByVal c As ubyte) As ubyte
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
    desp=rotorDefinition(tipo+2,(26+c+rotPos-ring) Mod 26)
    c=(c+desp) Mod 26
    
    tipo=rotorSetup(1,0)
    ring=rotorSetup(1,1)
    rotPos=rotorSetup(1,2)
    desp=rotorDefinition(tipo+2,(26+c+rotPos-ring) Mod 26)
    c=(c+desp) Mod 26
    
    tipo=rotorSetup(0,0)
    ring=rotorSetup(0,1)
    rotPos=rotorSetup(0,2)
    desp=rotorDefinition(tipo+2,(26+c+rotPos-ring) Mod 26)
    c=(c+desp) Mod 26
    
    desp=rotorDefinition(reflector,c Mod 26) ' 0 o  1 segun el reflector'
    c=(c+desp) Mod 26
    
    tipo=rotorSetup(0,0)
    ring=rotorSetup(0,1)
    rotPos=rotorSetup(0,2)
    desp=inverseRotorDefinition(tipo+2,(26+c+rotPos-ring) Mod 26)
    c=(26+c-desp) Mod 26
    
    tipo=rotorSetup(1,0)
    ring=rotorSetup(1,1)
    rotPos=rotorSetup(1,2)
    desp=inverseRotorDefinition(tipo+2,(26+c+rotPos-ring) Mod 26)
    c=(26+c-desp) Mod 26
    
    tipo=rotorSetup(2,0)
    ring=rotorSetup(2,1)
    rotPos=rotorSetup(2,2)
    desp=inverseRotorDefinition(tipo+2,(26+c+rotPos-ring) Mod 26)
    c=(26+c-desp) Mod 26
    
    n=0
    Do
        if plugBoard(n*2)=c And plugBoard(n*2+1)<99
        c=plugBoard((n*2)+1)
        n=10
        end If
        if plugBoard(n*2+1)=c And plugBoard(n*2)<99 And n<10
        c=plugBoard(n*2)
        n=10
        end If
        n=n+1
    Loop While n<10
    
    Return c
End Function


datos:
asm
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


