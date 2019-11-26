

% connected(x,y,c)
% is true if the robot can move from location x to location y with a cost of c

% the following is defined for Map I on google doc
% e.g. robots can go from a to c1 with a cost of 2:
connected(a, c1, 2).
connected(c1, a, 3).
connected(c1, b, 1).
connected(b, c1, 2).
connected(b, c3, 1).
connected(c3, b, 3).
connected(c1, c2, 4).
connected(c2, c1, 2).


% order(c,q,r,u)
% is true if a customer at location c has ordered q units of food from restaurants at r, and the urgency level is u.

% the following is defined for Map I
% e.g. customer at c3 has ordered 2 units of food from A. This order is urgent.
order(c3, 2, a, urgent).
order(c2, 1, b, not_urgent).
order(c1, 1, b, not_urgent).

