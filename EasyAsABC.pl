:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(clpfd)).


%%DEFINITIONS

emptyBoard([[-1,-1,-1,-1],
[-1,-1,-1,-1],
[-1,-1,-1,-1],
[-1,-1,-1,-1]]).

testBoard([[1,0,0,0],
[0,3,0,0],
[0,0,0,0],
[0,4,0,0]]).

testLineRestrictions1([-1,2,-1,3]).
testLineRestrictions2([1,-1,-1,-1]).
testLineRestrictions3([2,-1,3,-1]).
testLineRestrictions4([-1,-1,-1,1]).


/* Start Menu */

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

convertNum(' ', -1).                   
convertNum('X', 0).
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

generateRandomRestrictions(List, Final, Final, List).
        
generateRandomRestrictions(R, Size, Final, List):-
        random(0, Size, E1),
        append(List, [E1], Result),
        Final1 is Final+1,
        generateRandomRestrictions(R, Size, Final1, Result). 
        
        
startDynamic(L):-
        generateRandomRestrictions(R1, L, 0, []),
        generateRandomRestrictions(R2, L, 0, []),
        generateRandomRestrictions(R3, L, 0, []),
        generateRandomRestrictions(R4, L, 0, []),       
        dynamicGame(Board, R1, R2, R3, R4, L),      
        L1 is L+1,        
        createBoard(Board, [], [], Scene, L1, 1, 1),
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
       %% constrainSides(Board, R1, R2, R3, R4), 
        append(Scene, BoardOut),
       
        labeling([], BoardOut).   

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
       
%%constrainSides(Board, R1, R2, R3, R4):-
        %%constrainUp(Board, R1),
        %%constrainRight(Board, R2),
        %%constrainDown(Board, R3),
        %%constrainLeft(Board, R4).

%%constrainUp([Row|Rest], R1).
        
		