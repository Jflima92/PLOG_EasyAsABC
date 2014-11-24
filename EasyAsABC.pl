:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(clpfd)).


%%DEFINITIONS

emptyBoard([[-1,-1,-1,-1],
[-1,-1,-1,-1],
[-1,-1,-1,-1],
[-1,-1,-1,-1]]).

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

printBoard([],5, _, _).
printBoard([L1|Resto],I, Line2, Line4) :-
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
        printBoard(Resto, I2, Line2, Line4).

printLineRestrictions(Line):-
       printLine(Line).
  

printInitialBoard:-
        emptyBoard(Cenas),
        nl,
        printScenario(Cenas).

printScenario(Board):- 
        testLineRestrictions1(Line1),
        testLineRestrictions2(Line2),
        testLineRestrictions3(Line3),
        testLineRestrictions4(Line4),
        print('  '),
        printLineX(Line1),
        nl,        
        printBoard(Board, 1, Line2, Line4),        
        print('  '),
        printLineX(Line3),nl. 

fazCoisas(Board):-                              %%Being R1 Top, R2 Right, R3 Down , R4 Left.
        Board = [A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16],
        domain(Board,0, 3),
        all_distinct([A1,A2,A3,A4]),
        all_distinct([A5,A6,A7,A8]),
        all_distinct([A9, A10, A11, A12]),
        all_distinct([A13, A14, A15, A16]),
        
        all_distinct([A1,A5,A9,A13]),
        all_distinct([A2,A6,A10,A14]),
        all_distinct([A3, A7, A11, A15]),
        all_distinct([A4, A8, A12, A16]),
        
        labeling([], Board).

createBoard([], _, BoardOut, BoardOut, 1, 5).

createBoard(Rest, Accum, Line, BoardOut, 5, I2):-
        append(Line, [Accum], Accccum),
        IY is I2+1,
        createBoard(Rest, [], Accccum, BoardOut, 1 , IY).

createBoard([P|Rest], Accum, Line, BoardOut, I, I2):-
        I < 6,
        append(Accum, [P], Result),
        IX is I+1,
        createBoard(Rest, Result, Line, BoardOut, IX, I2).             


printCoisas:-
        fazCoisas(Board),
        createBoard(Board, [], [], Scene, 1, 1),
        printScenario(Scene).
        
             
        


