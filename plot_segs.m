% --- 1. Import the data ---
fileName = 'edges.txt'; % Replace with your actual file name
data = dlmread(fileName); % Reads the file into a matrix
betas = dlmread('betas.txt')
% Alternatively, use readmatrix (available in newer MATLAB versions):
% data = readmatrix(fileName);

% The data matrix now has 4 columns:
% Column 1: x_start
% Column 2: y_start
% Column 3: x_end
% Column 4: y_end
c=cos(pi/4);
X = [-1.0,0.0,1.0,0.0];
Y = [0,-2,0,2];
X=[-c,0,c,0,c+3];
Y=[0,-c,0,c,0];
x_start = data(:, 1)'; % All rows, 1st column
y_start = data(:, 2)'; % All rows, 2nd column
x_end = data(:, 3)';   % All rows, 3rd column
y_end = data(:, 4)';   % All rows, 4th column
eepsvr = data(:,5)';
eepsch = data(:,6)';
% --- 2. Prepare data for plotting line segments ---
% To plot N segments, you need an X-vector and a Y-vector, each with 2*N + 1 elements.
% Each segment is defined by [x_start, x_end, NaN] and [y_start, y_end, NaN].
% The 'NaN' is crucial; it breaks the line between segments so they aren't connected.

% Reshape the start and end points into a structure suitable for the plot function:
% X-coordinates: [x1_start, x1_end, NaN, x2_start, x2_end, NaN, ...]
X_coords = [x_start, x_end, nan(size(x_start))];
X_plot = X_coords'; % Transpose to get a single column vector
X_plot = X_plot(:); % Convert to a single column vector

% Y-coordinates: [y1_start, y1_end, NaN, y2_start, y2_end, NaN, ...]
Y_coords = [y_start, y_end, nan(size(y_start))];
Y_plot = Y_coords'; % Transpose to get a single column vector
Y_plot = Y_plot(:); % Convert to a single column vector

% --- 1. Import the data ---
fileName = 'faces.txt'; % Replace with your actual file name
data = dlmread(fileName); % Reads the file into a matrix
fepsvr = data(:,7)';
fepsch = data(:,8)';
%leps = sort(unique([eepsvr,eepsch,fepsvr,fepsch,.5]));
disp (leps);
% Correctly reshape the data for plotting:
% N rows (triangles) x 6 columns (x1 y1 x2 y2 x3 y3)
num_triangles = size(data, 1);

%% 2. Prepare Vertex Coordinates
% Initialize matrices to hold all X and Y coordinates for all vertices
% Each row will contain [x1, x2, x3] or [y1, y2, y3]
X_coords = zeros(num_triangles, 3);
Y_coords = zeros(num_triangles, 3);

% Extract the coordinates based on the (x1, y1, x2, y2, x3, y3) format
X_coords(:, 1) = data(:, 1); % x1
X_coords(:, 2) = data(:, 3); % x2
X_coords(:, 3) = data(:, 5); % x3

Y_coords(:, 1) = data(:, 2); % y1
Y_coords(:, 2) = data(:, 4); % y2
Y_coords(:, 3) = data(:, 6); % y3

leps = betas(:,1);
for j=1:length(leps)
  % --- 3. Plot the line segments ---
  fig= figure; % Create a new figure window
  
  subplot(1,2,1);
  
  k=length(eepsch(eepsch<=leps(j)));
  plot([x_start(1:k);x_end(1:k)], [y_start(1:k); y_end(1:k)], '--b', 'LineWidth', .5); % Plot as blue lines
  hold on;
  scatter(X, Y, 50, 'black', 'filled');
  colors = jet(num_triangles); 
  
  for i = 1:num_triangles
    if fepsch(i)>leps(j) 
      break;
    end
      % Use 'patch' to plot a filled 2D polygon (the triangle)
      patch(X_coords(i, :), Y_coords(i, :), colors(i, :), ...
            'EdgeColor', 'none',  'FaceAlpha', 0.6); 
      
      % Note: In 2D, the third argument to patch defines the color, 
      % not Z coordinates.
  end
  
  % --- 4. Add labels and title (optional but recommended) ---
  xlabel("beta2="+num2str(betas(j,2))+", beta1="+num2str(betas(j,3))+", beta0="+num2str(betas(j,6)));
  ylabel('Y');
  title("Čech, epsilon = " +num2str(leps(j)));
  grid on;
  axis equal; % Ensure proper aspect ratio for visualization
  subplot(1,2,2);

  k=length(eepsvr(eepsvr<=leps(j)));
  if k>0
    plot([x_start(1:k);x_end(1:k)], [y_start(1:k); y_end(1:k)], '--b', 'LineWidth', .5); % Plot as blue lines
  end
  hold on;
  scatter(X, Y, 50, 'black', 'filled');
  for i = 1:num_triangles
    if fepsvr(i)>leps(j) 
      break;
    end
      % Use 'patch' to plot a filled 2D polygon (the triangle)
      patch(X_coords(i, :), Y_coords(i, :), colors(i, :), ...
            'EdgeColor', 'none',  'FaceAlpha', 0.6); 
      
      % Note: In 2D, the third argument to patch defines the color, 
      % not Z coordinates.
  end
  % --- 4. Add labels and title (optional but recommended) ---
  xlabel("beta2="+num2str(betas(j,4))+", beta1="+num2str(betas(j,5))+", beta0="+num2str(betas(j,7)));
  ylabel('Y');
  title("Viettoris-Rips, epsilon = " +num2str(leps(j)));
  grid on;
  axis equal; % Ensure proper aspect ratio for visualization
  hold off;
  saveas(fig,"fig"+num2str(j)+".pdf")
end