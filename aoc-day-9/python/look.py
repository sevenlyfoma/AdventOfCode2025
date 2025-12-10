import matplotlib.pyplot as plt
import numpy as np

f = open("test.txt")
# f = open("test2.txt")
# f = open("input.txt")
lines = f.readlines()

reds = []

for line in lines:
    xs = line.strip().split(",")
    res = map(int, xs)
    reds.append(list(res))

print(reds)

greenlines = []
for i in range(len(reds)):
    greenlines.append([reds[i], reds[(i+1)%(len(reds))]])

print(greenlines)

points_array = np.array(reds)
x_coords, y_coords = points_array.T

for line_segment in greenlines:
    # line_segment is like: [[x1, y1], [x2, y2]]
    
    # Extract the coordinates of the start and end points
    start_point = line_segment[0] # e.g., [1, 10]
    end_point = line_segment[1]   # e.g., [3, 2]
    
    # Separate the X-coordinates and Y-coordinates for plt.plot()
    x_coords = [start_point[0], end_point[0]] # [x1, x2] -> [1, 3]
    y_coords = [start_point[1], end_point[1]] # [y1, y2] -> [10, 2]
    
    # Plot the line segment
    # The default behavior of plt.plot() is to draw a line connecting the points.
    plt.plot(x_coords, y_coords, 
             linestyle='-',  # Use a solid line     # Place a marker (dot) at the endpoints
             color='blue',   # Set line color
             alpha=0.7)      # Slightly transparent
    
# """(13160,81186)
# (86887,18772)
# (13160,18772)
# (86887,81186)"""

# """
# (93680,70479)
# (6787,69545)
# """

# real = [[[93680,70479],[6787,69545]]]


# real = [[[9,1],[11,7]]]
# real = [[[5424,67450],[94703,50308]]]

# for line_segment in real:
#     # line_segment is like: [[x1, y1], [x2, y2]]
    
#     # Extract the coordinates of the start and end points
#     start_point = line_segment[0] # e.g., [1, 10]
#     end_point = line_segment[1]   # e.g., [3, 2]
    
#     # Separate the X-coordinates and Y-coordinates for plt.plot()
#     x_coords = [start_point[0], end_point[0]] # [x1, x2] -> [1, 3]
#     y_coords = [start_point[1], end_point[1]] # [y1, y2] -> [10, 2]
    
#     # Plot the line segment
#     # The default behavior of plt.plot() is to draw a line connecting the points.
#     plt.plot(x_coords, y_coords, 
#              linestyle='-',  # Use a solid line     # Place a marker (dot) at the endpoints
#              color='red',   # Set line color
#              alpha=0.7)      #


plt.scatter(x_coords, y_coords, marker='x', s=200)
plt.show()
