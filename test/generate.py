from PIL import Image

def create_image():
    # Create a blank white image
    width, height = 2, 4
    image = Image.new("RGB", (width, height), "white")

    # Set a specific pixel to black (0, 0 is the top-left corner)
    pixel_position = (0, 0)  # Change this to the desired pixel position
    image.putpixel(pixel_position, (0, 0, 0))  # Set the pixel to black
    pixel_position = (1, 3)  # Change this to the desired pixel position
    image.putpixel(pixel_position, (0, 0, 0))  # Set the pixel to black
    pixel_position = (0, 2)  # Change this to the desired pixel position
    image.putpixel(pixel_position, (0, 0, 0))  # Set the pixel to black

    # Save the image as a PNG file
    image.save("output.png")

if __name__ == "__main__":
    create_image()