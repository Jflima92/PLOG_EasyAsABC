:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(clpfd)).
:- use_module(library(system)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%  Definitions

convertNum(' ', -1).                   
convertNum(' ', 0).
convertNum('A',1).
convertNum('B',2).
convertNum('C',3).
convertNum('D',4).
convertNum('E',5).
convertNum('F',6).
convertNum('G',7).
convertNum('H',8).
convertNum('I',9).
convertNum('J',10). 
convertNum('K',11).
convertNum('L',12).
convertNum('M',13).
convertNum('N',14).
convertNum('O',15).
convertNum('P',16).
convertNum('Q',17).
convertNum('R',18).
convertNum('S',19).
convertNum('T',20).
convertNum('U',21).
convertNum('V',22).
convertNum('W',23).
convertNum('Y',24).
convertNum('Z',25).

emptyBoard([[-1,-1,-1,-1],
[-1,-1,-1,-1],
[-1,-1,-1,-1],
[-1,-1,-1,-1]]).

testBoard([[1,0,0,0],
[0,3,0,0],
[0,0,0,0],
[0,4,0,0]]).

testLineRestrictions1([-1,2,-1,1,-1]).
testLineRestrictions2([1,-1,1,-1,3]).
testLineRestrictions3([2,-1,3,-1,3]).
testLineRestrictions4([-1,3,-1,1,-1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%  Start Menu

intro:-
nl,
print('                 Easy as ABC').

start:-
        
         emptyBoard(Board),
         startmenu(Board).

startmenu(Board) :-
       
set_prolog_flag(fileerrors,off), intro, nl, nl,
        write('1 - Generate and Solve NxN Game Board'), nl,
        write('0 - Exit'), nl,
       
        repeat, read(Op), Op >= 0, Op =< 12,!,
        menu(Op, Board), repeat, skip_line, get_code(_), startmenu(Board).

menu(0):- 
abort.

menu(1, Board):-
        nl,
        write('What is the length of the Board you want to generate? (NxN) '),nl,
        read(Size),
        startDynamic(Size);
        startmenu(Board).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%  Print Board and Scenario Predicates

printLine([]).

printLine([P1|Resto]):-      
        convertNum(Val, P1),
        print(' '),
        print('|'),
        print(Val),
        printLine(Resto).

printLineX([]).

printLineX([P1|Resto]):-      
        convertNum(Val, P1),
        print(' '),
        print(Val),
        print(' '),
        printLineX(Resto).

printBoard([],Size,Size, _, _).
printBoard([L1|Resto],Size,I, Line2, Line4) :-
        nth1(I, Line4, Elem),  
        convertNum(Val, Elem),
        print(Val),
        printLine(L1),        
        print(' | '),        
        nth1(I, Line2, Elem1),
        convertNum(Val1, Elem1),  
        print(Val1),
        nl,
        I2 is I+1,
        printBoard(Resto,Size, I2, Line2, Line4).

printLineRestrictions(Line):-
       printLine(Line).

printScenario(Board, Size, R1, R2, R3, R4):- 
        
        print('  '),
        printLineX(R1),
        nl,        
        printBoard(Board, Size, 1, R2, R4),        
        print('  '),
        printLineX(R3),nl. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%  NxN Board Creation

createBoard([], _, BoardOut, BoardOut, Size, 1, Size).

createBoard(Rest, Accum, Line, BoardOut, Size, Size, I2):-
        append(Line, [Accum], Accccum),
        IY is I2+1,
        createBoard(Rest, [], Accccum, BoardOut, Size, 1 , IY).

createBoard([P|Rest], Accum, Line, BoardOut, Size, I, I2):-
        
        I < Size+1,
        append(Accum, [P], Result),
        IX is I+1,
        createBoard(Rest, Result, Line, BoardOut, Size, IX, I2).  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%  Constraint side corners verification                                  NOT AT USE 

leftTopCheck(Restrictions, Size):-
        element(1, Restrictions, Upper),
        Pos is Size*3+1,
        element(Pos, Restrictions, Left),
        Left #= Upper.

leftBottomCheck(Restrictions, Size):-
        BotPos is Size*2+1,
        element(BotPos, Restrictions, Bottom),
        Pos is Size*4,
        element(Pos, Restrictions, Left),
        Left #= Bottom.

rightBottomCheck(Restrictions, Size):-
        BotPos is Size*3,
        element(BotPos, Restrictions, Bottom),
        Pos is Size*2,
        element(Pos, Restrictions, Right),
        Right #= Bottom.

rightTopCheck(Restrictions, Size):-
        TopPos is Size,
        element(TopPos, Restrictions, Top),
        Pos is Size+1,
        element(Pos, Restrictions, Right),
        Right #= Top.
        

cornersVerification(Restrictions, Size):-
        leftTopCheck(Restrictions, Size),
        leftBottomCheck(Restrictions, Size),
        rightBottomCheck(Restrictions, Size),
        rightTopCheck(Restrictions, Size).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%  Opposite side same pos cannot be the same constraint predicates 

getRowByPos(_, Size2, Size, Accum, Accum):-
        Size2 #= Size+1.

getRowByPos(Restrictions, I, Size, Accum, Rest):-
        element(I, Restrictions, Elem),
        append(Accum, [Elem], Out),
        I2 is I+1,
        getRowByPos(Restrictions, I2, Size, Out, Rest).

constrainSides(_, _, Size2, Size):-
        Size2 #= Size+1.

constrainSides(Rest1, Rest2, I, Size):-
        element(I, Rest1, Elem1),
        element(I, Rest2, Elem2),
        
        Div1 is round(Size/2+1),
        Div is round(Size/2),
        nvalue(Div1, Rest1),
        nvalue(Div1, Rest2),                            %%nvalue com erros em boards impares
        count(0, Rest1, #=, Div),
        count(0, Rest2, #=, Div),
        Elem1 #\= Elem2 #/\ ((Elem1 #= 0 #/\ Elem2 #\= 0) #\/ (Elem1 #\= 0 #/\ Elem2 #= 0)),
        I2 is I+1,
        constrainSides(Rest1, Rest2, I2, Size).
        

topBotVerification(Restrictions, Size):-
        getRowByPos(Restrictions, 1, Size, [], RestriTop),
        PosI is Size*2+1,
        PosF is Size*3,
        getRowByPos(Restrictions,  PosI, PosF, [], RestriBot),
        constrainSides(RestriTop, RestriBot, 1, Size).

leftRightVerification(Restrictions, Size):-
        PosIR is Size+1,
        PosFR is Size*2,
        getRowByPos(Restrictions, PosIR, PosFR, [], RestriRight),
        PosIL is Size*3+1,
        PosFL is Size*4,
        getRowByPos(Restrictions, PosIL, PosFL, [], RestriLeft),
        constrainSides(RestriRight, RestriLeft, 1, Size).        

oppositeSideVerification(Restrictions, Size):-
        topBotVerification(Restrictions, Size),
        leftRightVerification(Restrictions, Size).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%  Random Side Constraints generation predicates           

generateRandomRestrictions(List, Final, Final, List).

generateRandomRestrictions(R, Size, Final, List):-
        random(0, Size, E1),
        \+member(E1, List),
        append(List, [E1], Result),
        Final1 is Final+1,
        generateRandomRestrictions(R, Size, Final1, Result).
    
generateRandomRestrictions(R, Size, Final, List):-
        random(0, Size, E1),
        member(E1, List),
        generateRandomRestrictions(R, Size, Final, List).

placeRandomValueInEachRow(_, _,Size2, Size):-
        Size2 #= Size +1.

placeRandomValueInEachRow(Restrictions, C, I, Size):-
        Limit is I*Size+1,
        Start is Size*C+1,
        random(Start, Limit, Pos),    %%check initial value   
        random(1, Size, Value),
        element(Pos, Restrictions, Value),
        C2 is C+1,
        I2 is I+1,
        placeRandomValueInEachRow(Restrictions, C2, I2, Size).

generateRandomPlainRestrictions(Restrictions, Size):-
        N is Size * Size,
        length(Restrictions, N),
        S is Size-1,
        domain(Restrictions, 0,  S),
        
        placeRandomValueInEachRow(Restrictions, 0, 1, Size),
        oppositeSideVerification(Restrictions, Size),
        cornersVerification(Restrictions, Size),    
        labeling([], Restrictions).          
        

generator(Restriction, Size):-
        length(Restriction, Size),
        Size1 is Size-1,
        domain(Restriction, 0, Size1),
        Div is round(Size/2),
        count(0, Restriction, #>=, Div),
        labeling([], Restriction). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%  Solver predicates

getRow([Row|Rest], Rest, Row).
        
startDynamic(L):-
        now(Secs),
        setrand(Secs),
        L1 is L+1,
        
       /* testLineRestrictions1(R1),
        testLineRestrictions2(R2),
        testLineRestrictions3(R3),
        testLineRestrictions4(R4),*/
        
        generateRandomPlainRestrictions(Restrictions, L),        
        createBoard(Restrictions, [], [], RestList, L1, 1, 1),
       
        getRow(RestList, Rest, R1),
        getRow(Rest, Rest1, R2),
        getRow(Rest1, Rest2, R3),
        getRow(Rest2, _, R4),
        emptyBoard(A),nl,
        printScenario(A, L1, R1, R2, R3, R4),nl,
        
        dynamicGame(Board, R1,R2,R3,R4, L),      
                
        createBoard(Board, [], [], Scene, L1, 1, 1),
        write('Solution:'),nl,nl,
        printScenario(Scene, L1, R1, R2, R3, R4),
        startmenu(Scene).
        
  dynamicGame(Board, R1, R2, R3, R4, L):-                                 %%Verify board lentgh verification because its failling.
        L1 is L*L,
        length(Board, L1), 
        DL is L-1,
        domain(Board, 0, DL),
        L2 is L+1, 
        createBoard(Board, [], [], Scene, L2, 1, 1),
        constrainRows(Scene, L),
        constrainColumns(Scene, L),
        constrainSides(Scene, R1, R2, R3, R4, L), 
        append(Scene, BoardOut),
       
        \+labeling([], BoardOut),
        write('There is no solution for the generated board. Trying again... '),nl,nl,
        startDynamic(L).
      
dynamicGame(Board, R1, R2, R3, R4, L):-                                 %%Verify board lentgh verification because its failling.
        L1 is L*L,
        length(Board, L1), 
        DL is L-1,
        domain(Board, 0, DL),
        L2 is L+1, 
        createBoard(Board, [], [], Scene, L2, 1, 1),
        constrainRows(Scene, L),
        constrainColumns(Scene, L),        
        constrainSides(Scene, R1, R2, R3, R4, L), 
        append(Scene, BoardOut),       
        labeling([], BoardOut).   



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%  Board placement constraints

constrainRows(Board, L):-
        checkRows(Board, L).

checkRows([], _).
checkRows([Row|Rest], L):-
        all_distinct(Row),
        checkRows(Rest, L).
        
constrainColumns(Board, Size):-
        analyzeColumns(Board, Board, 1, 0, Size, []).

analyzeColumns(_, _, Size2, _, Size, _):-
        Size2 #= Size+1.

analyzeColumns(Scene, _, Count, Size, Size, Column):-
        all_distinct(Column),
        Count1 is Count+1,
        analyzeColumns(Scene, Scene, Count1, 0, Size, []).
                

analyzeColumns(Scene, [Row|Rest], Count, I, Size, Column):-
        I2 is I+1,
        element(Count, Row, Elem),
        append(Column, [Elem], Res),        
        analyzeColumns(Scene, Rest, Count, I2, Size, Res).       
        
       
constrainSides(Board, R1, R2, R3, R4, Size):-
        constrainUp(Board, R1, Size),
        constrainRight(Board, R2, Size),
        constrainDown(Board, R3, Size),
        constrainLeft(Board, R4, Size).

constrainLeft(Board, R4, Size):-
        getLastColumn(Board, 0, 1, [], Column1),
        getLastColumn(Board, 0, 2, [], Column2),
        constrain(Column1, Column2, R4, 1, Size).
        

getLastRow([RowOut|_], Size, Size, RowOut).

getLastRow([_|Rest], I, Size, RowOut):-
        I2 is I+1,
        getLastRow(Rest, I2, Size, RowOut).

constrainDown(Board, R3, Size):-
        getLastRow(Board, 1, Size, Row1),
        Size2 is Size-1,
        getLastRow(Board, 1, Size2, Row2),
        constrain(Row1, Row2, R3, 1, Size).
        

getLastColumn([], _, _, Col, Col).
getLastColumn([Row|Rest], I, Size, Col, Column):-
        I2 is I+1,
        element(Size, Row, Elem),
        append(Col, [Elem], Res),
        getLastColumn(Rest, I2, Size, Res, Column).

constrainRight(Board, R2, Size):-
        getLastColumn(Board, 0, Size, [], Column1),
        Size2 is Size-1,
        getLastColumn(Board, 0, Size2, [], Column2),
        constrain(Column1, Column2, R2, 1, Size).


getFirstRow([Elem|Rest], Elem, Rest). 

constrainUp(Board, R1, Size):-
        getFirstRow(Board, Row1, B1),        
        getFirstRow(B1, Row2, _),
        constrain(Row1, Row2, R1, 1, Size).
       
equals(Elem, Elem).

constrain(_, _, _, Size2, Size):-
        Size2 #= Size +1.

constrain(Row1, Row2, R1, I, Size):-        
        I2 is I+1,
        element(I, R1, R),
        equals(R, -1),
        constrain(Row1, Row2, R1, I2, Size).

constrain(Row1, Row2, R1, I, Size):-        
        I2 is I+1,
        element(I, Row1, ER1),
        element(I, Row2, ER2),
        element(I, R1, R),
        (ER1 #= 0 #/\ ER2 #= R) #\/ ER1 #= R ,
        
        constrain(Row1, Row2, R1, I2, Size).            