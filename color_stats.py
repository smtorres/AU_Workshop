import cv2
import numpy as np
import matplotlib.pyplot as plt

def plot_color_histogram(image):
    # Convert the image from BGR to RGB
    image_rgb = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
    
    # Calculate histograms for each channel
    hist_red = cv2.calcHist([image], [0], None, [256], [0,256])
    hist_green = cv2.calcHist([image], [1], None, [256], [0,256])
    hist_blue = cv2.calcHist([image], [2], None, [256], [0,256])
    
    # Plot histograms
    plt.figure(figsize=(10, 5))
    plt.title("Color Histogram")
    plt.xlabel("Bins")
    plt.ylabel("# of Pixels")
    plt.plot(hist_red, color="red", label="Red")
    plt.plot(hist_green, color="green", label="Green")
    plt.plot(hist_blue, color="blue", label="Blue")
    plt.legend()
    plt.show()

def calculate_color_stats(image):
    # Split the image into color channels
    b, g, r = cv2.split(image)
    
    # Calculate mean, median, and standard deviation for each channel
    mean_values = {'Red': np.mean(r), 'Green': np.mean(g), 'Blue': np.mean(b)}
    median_values = {'Red': np.median(r), 'Green': np.median(g), 'Blue': np.median(b)}
    std_values = {'Red': np.std(r), 'Green': np.std(g), 'Blue': np.std(b)}
    
    return mean_values, median_values, std_values

# Load the image
image_path = '/Users/michelletorres/Dropbox/UCLA/CLASS_MATERIAL/IMAGE_ANALYSIS_WORKSHOP/Slides/Figures/Figures/Color_00.jpg'
image = cv2.imread(image_path)

# Display the image
plt.imshow(cv2.cvtColor(image, cv2.COLOR_BGR2RGB))
plt.axis('off')
plt.title('Original Image')
plt.show()

# Plot color histogram
plot_color_histogram(image)

# Calculate color statistics
mean_values, median_values, std_values = calculate_color_stats(image)
print("Mean values per channel:", mean_values)
print("Median values per channel:", median_values)
print("Standard deviation per channel:", std_values)



# import the necessary packages
from sklearn.cluster import KMeans
import matplotlib.pyplot as plt
import argparse
import utils
import cv2
# construct the argument parser and parse the arguments

# load the image and convert it from BGR to RGB so that
# we can dispaly it with matplotlib
image = cv2.imread(image_path)
image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)

image = image.reshape((image.shape[0] * image.shape[1], 3))

n_clusters = 10
clt = KMeans(n_clusters = n_clusters)
clt.fit(image)

# import the necessary packages
import numpy as np
import cv2
def centroid_histogram(clt):
    # grab the number of different clusters and create a histogram
    # based on the number of pixels assigned to each cluster
    numLabels = np.arange(0, len(np.unique(clt.labels_)) + 1)
    (hist, _) = np.histogram(clt.labels_, bins = numLabels)
    # normalize the histogram, such that it sums to one
    hist = hist.astype("float")
    hist /= hist.sum()
    # return the histogram
    return hist

def plot_colors(hist, centroids):
    # initialize the bar chart representing the relative frequency
    # of each of the colors
    bar = np.zeros((50, 300, 3), dtype = "uint8")
    startX = 0
    # loop over the percentage of each cluster and the color of
    # each cluster
    for (percent, color) in zip(hist, centroids):
        # plot the relative percentage of each cluster
        endX = startX + (percent * 300)
        cv2.rectangle(bar, (int(startX), 0), (int(endX), 50),
            color.astype("uint8").tolist(), -1)
        startX = endX
    
    # return the bar chart
    return bar

hist = centroid_histogram(clt)
bar = plot_colors(hist, clt.cluster_centers_)
# show our color bart
plt.figure()
plt.axis("off")
plt.imshow(bar)
plt.show()
# show our image
plt.figure()
plt.axis("off")
plt.imshow(image)