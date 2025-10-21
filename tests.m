%%
figure;
X1 = rand(1, 10);
X2 = rand(1, 10);
Y1 = rand(1, 10);
Y2 = rand(1, 10);

disp([X1; X2]);

plot([X1; X2], [Y1; Y2]) 

%%
% Define the numeric and string components
name = 'Project Alpha';
version_number = 3.5;

% 1. Convert the number to a string using num2str
version_str = num2str(version_number);

% 2. Concatenate the strings using square brackets []
output_message = ['Report Name: ', name, ', Version: ', version_str];

disp(output_message);
%%
% Define the numeric and string components using modern string type ("")
name = "Sensor Data";
reading = 42; % Numeric value

% Use the '+' operator to concatenate strings and numbers.
% MATLAB automatically calls num2str() internally for the 'reading' variable.
output_string = "Data Set: " + name + ", Reading Count: " + reading;

disp(output_string);
%%
a = [0;2];
b = [-1;0];
c = [0;-2];
b1 = [1;0];

trad(a,b,c)
trad(a,b,b1)
%%



d= [1.5;0.];
l1=norm(a-d,2)
l2=norm(b-d,2)
l3=norm(c-d,2)
d1 = [0.;.75]
l1=norm(a-d1,2)
l2=norm(b-d1,2)
l3=norm(b1-d1,2)
function trad(a,b,c)
v1 = c-b;
v2 = a-b;
alpha = acos(dot(v1,v2)/norm(v1,2)/norm(v2,2));
l = norm(v2-v1,2);
trad= l/(2*sin(alpha))
end


