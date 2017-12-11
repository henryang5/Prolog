% Henry Ang
% CSC 3310 Concepts in Programming Language
% 11/6/17
% Prolog programming assignment
%
% Write a Prolog program that stores information about geometric objects. You must construct the
% geometric objects basing on simpler forms. The objects to construct are:
% 2D Point
% Segment, can be represented by two 2D Points, or one 2D Point, a slope and a length
% Square
% Rectangle
% Circle

% Given Objects
point2d(12, 2).
point2d(4, 13).
point2d(20 ,1).
segment(point2d(5, 4), point2d(5, 5)).
segment(point2d(4, 12), point2d(6, 10)).
rectangle(point2d(9, 16), point2d(16, 14)). 
rectangle(point2d(3, 6), point2d(10, 3)).  
square(point2d(3, 13), 4). 				
square(point2d(11, 6), 2). 	
circle(point2d(12, 4), 3). 

% Is this segment parallel to this other line? 
parallel(segment(point2d(X1, Y1), point2d(X2, Y2)), 
		 segment(point2d(X3, Y3), point2d(X4, Y4))) :-
	(((X1-X2)=:=0), ((X3-X4)=:=0));
	(Y1-Y2)/(X1-X2) =:= (Y3-Y4)/(X3-X4). 

% Is this segment perpendicular to this line? 
perpendicular(segment(point2d(X1, Y1), point2d(X2, Y2)), 
		 segment(point2d(X3, Y3), point2d(X4, Y4))) :-
	(Y1-Y2)/(X1-X2) =:= (-1/((Y3-Y4)/(X3-X4))).

% Is this segment contained in this polygon? 
contained(segment(point2d(X1, Y1), point2d(X2, Y2)),
			rectangle(point2d(A, B), point2d(C, D))) :- 
	(X1>=A), (X1=<C), (X2>=A), (X2=<C), (Y1=<B), (Y1>=D), (Y2=<B), (Y2>=D). 

contained(segment(point2d(X1, Y1), point2d(X2, Y2)), 
			square(point2d(A, B), L)) :- 
	(X1>=A), (X1=<(A+L)), (X2>=A), (X2=<(A+L)), (Y1=<B), (Y1>=(B-L)), (Y2=<B), (Y2>=(B-L)).
	
contained(segment(point2d(X1, Y1), point2d(X2, Y2)),
			circle(point2d(A, B), R)) :- 
	sqrt((X1-A)**2+(Y1-B)**2)=<R, 
	sqrt((X2-A)**2+(Y2-B)**2)=<R. 

% Is this polygon contained in this other polygon?
contained(square(point2d(X1, Y1), L), 
			circle(point2d(A, B), R)) :- 
	sqrt((X1-A)**2+(Y1-B)**2)=<R, 
	sqrt(((X1+L)-A)**2+((Y1-L)-B)**2)=<R. 

contained(circle(point2d(A, B), R), 
				square(point2d(X1, Y1), L)) :- 
	sqrt((X1-A)**2+(Y1-B)**2)>=R, 
	sqrt(((X1+L)-A)**2+((Y1-L)-B)**2)>=R. 

contained(square(point2d(X1, Y1), L), 
			rectangle(point2d(A, B), point2d(C, D))) :-  
    (X1>=A), (X1=<C), ((X1+L)>=A), ((X1+L)=<C), (Y1=<B), (Y1>=D), ((Y1-L)=<B), ((Y1-L)>=D).

contained(rectangle(point2d(X1, Y1), point2d(X2, Y2)),
				square(point2d(A, B), L)) :-  
    (X1>=A), (X1=<(A+L)), (X2>=A), (X2=<(A+L)), (Y1=<B), (Y1>=(B-L)), (Y2=<B), (Y2>=(B-L)).

contained(rectangle(point2d(A, B), point2d(C, D)), 
			circle(point2d(X1, Y1), R)) :- 
		(sqrt((X1-A)**2+(Y1-B)**2)=<R),
		(sqrt((X1-C)**2+(Y1-D)**2)=<R). 

contained(circle(point2d(X1, Y1), R), 
	rectangle(point2d(A, B), point2d(C, D))) :- 
		(sqrt((X1-A)**2+(Y1-B)**2)>=R),
		(sqrt((X1-C)**2+(Y1-D)**2)>=R). 


intersects(circle(point2d(X1,Y1),R), segment(point2d(A,B), point2d(C,D))) :-

((((sqrt((X1-A)**2+(Y1-B)**2)) * (sin(acos(((sqrt((X1-A)**2+(Y1-B)**2)**2) + (sqrt((A-C)**2+(B-D)**2)**2) - (sqrt((X1-C)**2+(Y1-D)**2)**2) ) 
		/ (2*(sqrt((X1-A)**2+(Y1-B)**2))*(sqrt((A-C)**2+(B-D)**2))))))) / sin(90)) < R).

% Does this polygon intersects this other polygon? 
intersects(rectangle(point2d(A, B), point2d(C, D)), 
			circle(point2d(X1, Y1), R)) :- 
	% is the center of the circle on the square? 
		((X1=:=A), (Y1=<B), (Y1>=D)); 
		((X1=:=C), (Y1=<B), (Y1>=D)); 
		((Y1=:=B), (X1>=A), (X1=<C)); 
		((Y1=:=D), (X1>=A), (X1=<C));  

	% is the height betweeen the segment and the center of the circle less than the raduis of the circle? 
		((((sqrt((X1-A)**2+(Y1-B)**2)) * (sin(acos(((sqrt((X1-A)**2+(Y1-B)**2)**2) + (sqrt((A-C)**2+(B-B)**2)**2) - (sqrt((X1-C)**2+(Y1-B)**2)**2) ) 
		/ (2*(sqrt((X1-A)**2+(Y1-B)**2))*(sqrt((A-C)**2+(B-B)**2))))))) / sin(90)) < R);

		((((sqrt((X1-A)**2+(Y1-B)**2)) * (sin(acos(((sqrt((X1-A)**2+(Y1-B)**2)**2) + (sqrt((A-A)**2+(B-D)**2)**2) - (sqrt((X1-A)**2+(Y1-D)**2)**2) ) 
		/ (2*(sqrt((X1-A)**2+(Y1-B)**2))*(sqrt((A-A)**2+(B-D)**2))))))) / sin(90)) < R);

		((((sqrt((X1-A)**2+(Y1-D)**2)) * (sin(acos(((sqrt((X1-A)**2+(Y1-D)**2)**2) + (sqrt((A-C)**2+(D-D)**2)**2) - (sqrt((X1-C)**2+(Y1-D)**2)**2) ) 
		/ (2*(sqrt((X1-A)**2+(Y1-D)**2))*(sqrt(((A-C)**2)+(D-D)**2))))))) / sin(90)) < R);

		((((sqrt((X1-C)**2+(Y1-B)**2)) * (sin(acos(((sqrt((X1-C)**2+(Y1-B)**2)**2) + (sqrt((C-C)**2+(B-D)**2)**2) - (sqrt((X1-C)**2+(Y1-D)**2)**2) ) 
		/ (2*(sqrt((X1-C)**2+(Y1-B)**2))*(sqrt((C-C)**2+(B-D)**2))))))) / sin(90)) < R).

intersects(circle(point2d(X1, Y1), R),
			rectangle(point2d(A, B), point2d(C, D))) :- 
	% is the center of the circle on the square? 
		((X1=:=A), (Y1=<B), (Y1>=D)); 
		((X1=:=C), (Y1=<B), (Y1>=D)); 
		((Y1=:=B), (X1>=A), (X1=<C)); 
		((Y1=:=D), (X1>=A), (X1=<C));  

	% is the height betweeen the segment and the center of the circle less than the raduis of the circle? 
		((((sqrt((X1-A)**2+(Y1-B)**2)) * (sin(acos(((sqrt((X1-A)**2+(Y1-B)**2)**2) + (sqrt((A-C)**2+(B-B)**2)**2) - (sqrt((X1-C)**2+(Y1-B)**2)**2) ) 
		/ (2*(sqrt((X1-A)**2+(Y1-B)**2))*(sqrt((A-C)**2+(B-B)**2))))))) / sin(90)) < R);

		((((sqrt((X1-A)**2+(Y1-B)**2)) * (sin(acos(((sqrt((X1-A)**2+(Y1-B)**2)**2) + (sqrt((A-A)**2+(B-D)**2)**2) - (sqrt((X1-A)**2+(Y1-D)**2)**2) ) 
		/ (2*(sqrt((X1-A)**2+(Y1-B)**2))*(sqrt((A-A)**2+(B-D)**2))))))) / sin(90)) < R);

		((((sqrt((X1-A)**2+(Y1-D)**2)) * (sin(acos(((sqrt((X1-A)**2+(Y1-D)**2)**2) + (sqrt((A-C)**2+(D-D)**2)**2) - (sqrt((X1-C)**2+(Y1-D)**2)**2) ) 
		/ (2*(sqrt((X1-A)**2+(Y1-D)**2))*(sqrt(((A-C)**2)+(D-D)**2))))))) / sin(90)) < R);

		((((sqrt((X1-C)**2+(Y1-B)**2)) * (sin(acos(((sqrt((X1-C)**2+(Y1-B)**2)**2) + (sqrt((C-C)**2+(B-D)**2)**2) - (sqrt((X1-C)**2+(Y1-D)**2)**2) ) 
		/ (2*(sqrt((X1-C)**2+(Y1-B)**2))*(sqrt((C-C)**2+(B-D)**2))))))) / sin(90)) < R).

intersects(square(point2d(A, B), L), 
				circle(point2d(X1, Y1), R)) :- 
	% is the center of the circle on the square? 
		((X1=:=A), (Y1=<B), (Y1>=(B-L))); 
		((X1=:=(A+L)), (Y1=<B), (Y1>=(B-L))); 
		((Y1=:=B), (X1>=A), (X1=<(A+L))); 
		(Y1=:=(B-L), (X1>=A), (X1=<(A+L)));

	% is the height betweeen the segment and the center of the circle less than the raduis of the circle? 
		((((sqrt((X1-A)**2+(Y1-B)**2)) * (sin(acos(((sqrt((X1-A)**2+(Y1-B)**2)**2) + (sqrt((A-(A+L))**2+(B-B)**2)**2) - (sqrt((X1-(A+L))**2+(Y1-B)**2)**2) ) 
		 / (2*(sqrt((X1-A)**2+(Y1-B)**2))*(sqrt((A-(A+L))**2+(B-B)**2))))))) / sin(90)) < R);

		((((sqrt((X1-A)**2+(Y1-B)**2)) * (sin(acos(((sqrt((X1-A)**2+(Y1-B)**2)**2) + (sqrt((A-A)**2+(B-(B-L))**2)**2) - (sqrt((X1-A)**2+(Y1-(B-L))**2)**2) ) 
		/ (2*(sqrt((X1-A)**2+(Y1-B)**2))*(sqrt((A-A)**2+(B-(B-L))**2))))))) / sin(90)) < R);

		((((sqrt((X1-A)**2+(Y1-(B-L))**2)) * (sin(acos(((sqrt((X1-A)**2+(Y1-(B-L))**2)**2) + (sqrt((A-(A+L))**2+((B-L)-(B-L))**2)**2) - (sqrt((X1-(A+L))**2+(Y1-(B-L))**2)**2) ) 
		/ (2*(sqrt((X1-A)**2+(Y1-(B-L))**2))*(sqrt(((A-(A+L))**2)+((B-L)-(B-L))**2))))))) / sin(90)) < R).

		%((((sqrt((X1-(A+L))**2+(Y1-B)**2)) * (sin(acos(((sqrt((X1-(A+L))**2+(Y1-B)**2)**2) + (sqrt(((A+L)-(A+L))**2+(B-(B-L))**2)**2) - (sqrt((X1-(A+L))**2+(Y1-(B-L))**2)**2) ) 
		%/ (2*(sqrt((X1-(A+L))**2+(Y1-B)**2))*(sqrt(((A+L)-(A+L))**2+(B-(B-L))**2)=)))))) / sin(90)) < R).

intersects(circle(point2d(X1, Y1), R),
			square(point2d(A, B), L)) :- 
	% is the center of the circle on the square? 
		((X1=:=A), (Y1=<B), (Y1>=(B-L))); 
		((X1=:=(A+L)), (Y1=<B), (Y1>=(B-L))); 
		((Y1=:=B), (X1>=A), (X1=<(A+L))); 
		(Y1=:=(B-L), (X1>=A), (X1=<(A+L)));

	% is the height betweeen the segment and the center of the circle less than the raduis of the circle? 
		((((sqrt((X1-A)**2+(Y1-B)**2)) * (sin(acos(((sqrt((X1-A)**2+(Y1-B)**2)**2) + (sqrt((A-(A+L))**2+(B-B)**2)**2) - (sqrt((X1-(A+L))**2+(Y1-B)**2)**2) ) 
		/ (2*(sqrt((X1-A)**2+(Y1-B)**2))*(sqrt((A-(A+L))**2+(B-B)**2))))))) / sin(90)) < R);

		((((sqrt((X1-A)**2+(Y1-B)**2)) * (sin(acos(((sqrt((X1-A)**2+(Y1-B)**2)**2) + (sqrt((A-A)**2+(B-(B-L))**2)**2) - (sqrt((X1-A)**2+(Y1-(B-L))**2)**2) ) 
		/ (2*(sqrt((X1-A)**2+(Y1-B)**2))*(sqrt((A-A)**2+(B-(B-L))**2))))))) / sin(90)) < R);

		((((sqrt((X1-A)**2+(Y1-(B-L))**2)) * (sin(acos(((sqrt((X1-A)**2+(Y1-(B-L))**2)**2) + (sqrt((A-(A+L))**2+((B-L)-(B-L))**2)**2) - (sqrt((X1-(A+L))**2+(Y1-(B-L))**2)**2) ) 
		/ (2*(sqrt((X1-A)**2+(Y1-(B-L))**2))*(sqrt(((A-(A+L))**2)+((B-L)-(B-L))**2))))))) / sin(90)) < R);

		((((sqrt((X1-(A+L))**2+(Y1-B)**2)) * (sin(acos(((sqrt((X1-(A+L))**2+(Y1-B)**2)**2) + (sqrt(((A+L)-(A+L))**2+(B-(B-L))**2)**2) - (sqrt((X1-(A+L))**2+(Y1-(B-L))**2)**2) ) 
		/ (2*(sqrt((X1-(A+L))**2+(Y1-B)**2))*(sqrt((((A+L)-(A+L))**2)+(B-(B-L))**2))))))) / sin(90)) < R).

intersects(rectangle(point2d(A, B), point2d(C, D)), 
			square(point2d(X1, Y1), L)) :- 
	% is any of the corner of the square on the rectangle?
		((X1=:=A), (Y1=<B), (Y1>=D)); 
		((X1=:=C), (Y1=<B), (Y1>=D)); 
		((Y1=:=B), (X1>=A), (X1=<C)); 
		((Y1=:=D), (X1>=A), (X1=<C)); 

		(((X1+L)=:=A), (Y1=<B), (Y1>=D)); 
		(((X1+L)=:=C), (Y1=<B), (Y1>=D)); 
		((Y1=:=B), ((X1+L)>=A), ((X1+L)=<C)); 
		((Y1=:=D), ((X1+L)>=A), ((X1+L)=<C)); 

		((X1=:=A), ((Y1-L)=<B), ((Y1-L)>=D)); 
		((X1=:=C), ((Y1-L)=<B), ((Y1-L)>=D)); 
		(((Y1-L)=:=B), (X1>=A), (X1=<C)); 
		(((Y1-L)=:=D), (X1>=A), (X1=<C)); 

		(((X1+L)=:=A), ((Y1-L)=<B), ((Y1-L)>=D)); 
		(((X1+L)=:=C), ((Y1-L)=<B), ((Y1-L)>=D)); 
		(((Y1-L)=:=B), ((X1+L)>=A), ((X1+L)=<C)); 
		(((Y1-L)=:=D), ((X1+L)>=A), ((X1+L)=<C)); 

	% is any corner of the square in the rectangle? 
		(X1>=A), (X1=<(A+L)), (Y1=<B), (Y1>=(B-L)); 
		((X1+L)>=A), ((X1+L)=<(A+L)), (Y1=<B), (Y1>=(B-L)); 
		(X1>=A), (X1=<(A+L)), ((Y1-L)=<B), ((Y1-L)>=(B-L)); 
		((X1+L)>=A), ((X1+L)=<(A+L)), ((Y1-L)=<B), ((Y1-L)>=(B-L)). 

intersects(square(point2d(X1, Y1), L),
			rectangle(point2d(A, B), point2d(C, D))) :- 
	% is any of the corner of the square on the rectangle?
		((X1=:=A), (Y1=<B), (Y1>=D)); 
		((X1=:=C), (Y1=<B), (Y1>=D)); 
		((Y1=:=B), (X1>=A), (X1=<C)); 
		((Y1=:=D), (X1>=A), (X1=<C)); 

		(((X1+L)=:=A), (Y1=<B), (Y1>=D)); 
		(((X1+L)=:=C), (Y1=<B), (Y1>=D)); 
		((Y1=:=B), ((X1+L)>=A), ((X1+L)=<C)); 
		((Y1=:=D), ((X1+L)>=A), ((X1+L)=<C)); 

		((X1=:=A), ((Y1-L)=<B), ((Y1-L)>=D)); 
		((X1=:=C), ((Y1-L)=<B), ((Y1-L)>=D)); 
		(((Y1-L)=:=B), (X1>=A), (X1=<C)); 
		(((Y1-L)=:=D), (X1>=A), (X1=<C)); 

		(((X1+L)=:=A), ((Y1-L)=<B), ((Y1-L)>=D)); 
		(((X1+L)=:=C), ((Y1-L)=<B), ((Y1-L)>=D)); 
		(((Y1-L)=:=B), ((X1+L)>=A), ((X1+L)=<C)); 
		(((Y1-L)=:=D), ((X1+L)>=A), ((X1+L)=<C)); 

	% is any corner of the square in the rectangle? 
		(X1>=A), (X1=<(A+L)), (Y1=<B), (Y1>=(B-L)); 
		((X1+L)>=A), ((X1+L)=<(A+L)), (Y1=<B), (Y1>=(B-L)); 
		(X1>=A), (X1=<(A+L)), ((Y1-L)=<B), ((Y1-L)>=(B-L)); 
		((X1+L)>=A), ((X1+L)=<(A+L)), ((Y1-L)=<B), ((Y1-L)>=(B-L)). 

% Is this segment vertical? 
vertical(segment(point2d(X, Y), point2d(X, Y2))). 

% Is this segment horitzontal? 
horizontal(segment(point2d(X1, Y), point2d(X2, Y))).

% Is this point on the figure?
on(point2d(X1, Y1), circle(point2d(A, B), R)) :- 
		sqrt((X1-A)**2+(Y1-B)**2)=:=R. 

on(point2d(X1, Y1), square(point2d(A, B), L)) :- 
		((X1=:=A), (Y1=<B), (Y1>=(B-L))); 
		((X1=:=(A+L)), (Y1=<B), (Y1>=(B-L))); 
		((Y1=:=B), (X1>=A), (X1=<(A+L))); 
		(Y1=:=(B-L), (X1>=A), (X1=<(A+L))). 

on(point2d(X1, Y1), rectangle(point2d(A, B), point2d(C, D))) :- 
		((X1=:=A), (Y1=<B), (Y1>=D)); 
		((X1=:=C), (Y1=<B), (Y1>=D)); 
		((Y1=:=B), (X1>=A), (X1=<C)); 
		((Y1=:=D), (X1>=A), (X1=<C)).  

on(point2d(A,B), segment(point2d(X1,Y1), point2d(X2,Y2))) :-
(   (((X1-X2)=:=0), (A=:=X1), (B=<Y1), (B>=Y2)) -> true 
;   (((X1-X2)=:=0), (A=:=X1), (B>=Y1), (B=<Y2)) -> true 
;   ((X1-X2)=:=0) -> false 
;   (B =:= (((Y1-Y2)/(X1-X2)* A) +  (Y1-((Y1-Y2)/(X1-X2)*X1))))
).

% Is this point in the figure?  
in(point2d(X1, Y1), circle(point2d(A, B), R)) :- 
		sqrt((X1-A)**2+(Y1-B)**2)=<R. 

in(point2d(X1, Y1), square(point2d(A, B), L)) :- 
		(X1>=A), (X1=<(A+L)), (Y1=<B), (Y1>=(B-L)). 

in(point2d(X1, Y1), rectangle(point2d(A, B), point2d(C, D))) :- 
		(X1>=A), (X1=<C), (Y1=<B), (Y1>=D). 