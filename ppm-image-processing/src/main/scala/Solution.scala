import util.{Pixel, Util}
import util.Util.{Image, GrayscaleImage, CharSequence}

object Solution {

    /**
     * <p> Parses a List of Chars to an Image.</p>
     *
     * @param image a List[Char] which represents a valid PPM format.
     * @return an Image (matrix of Pixels) built from the input image
     */
    def fromStringPPM(image: CharSequence): Image =
        val linearImage = image
            .foldRight(Nil: List[CharSequence])((c, acc) =>
                acc match {
                    case Nil => if (c == ' ' || c == '\n') Nil else List(List(c))
                    case x :: xs => if (c == ' ' || c == '\n') Nil :: acc else (c :: x) :: xs
                }
            )

        val (columns, rows) = linearImage
            .slice(1, 3)
            .map(_.mkString.toInt) match {
                case List(first, last) => (first, last)
                case _ => (0, 0)
            }

        val pixelsList = linearImage
            .drop(4)
            .map(_.mkString.toInt)
            .grouped(3)
            .map(pixelList => Pixel(pixelList.head, pixelList(1), pixelList(2)))
            .toList

        if (rows * columns != pixelsList.length) then Nil
        else pixelsList.grouped(columns).map(_.toList).toList

    /**
     * <p>Parses an Image type to a List[Char] in order to save in a file.</p>
     *
     * @param image a valid Image type to convert in a char sequence.
     * @return a char sequence built from image.
     */
    def toStringPPM(image: Image): CharSequence =
        ('P' :: '3' :: '\n' :: image.head.length.toString.toList)
            ++ (' ' :: image.length.toString.toList)
            ++ ('\n' :: '2' :: '5' :: '5' :: '\n' :: Nil)
            ++ image
                .flatMap(
                    _.flatMap(pixel =>
                        pixel.red.toString.toList
                        ++ (' ' :: pixel.green.toString.toList)
                        ++ (' ' :: pixel.blue.toString.toList)
                        ++ ('\n' :: Nil)
                    )
                )

    /**
     * <p>Concatenates two Images into one on the Oy axis.</p>
     *
     * <p>The new image will represent the second image
     * concatenated with the first image.</p>
     *
     * @param image1 first image to concatenate to.
     * @param image2 second image to concatenate from.
     * @return a new Image representing the concatenation of the two images.
     */
    def verticalConcat(image1: Image, image2: Image): Image =
        if image1.head.length != image2.head.length then Nil
        else image1 ++ image2

    /**
     * <p>Concatenates two Images into one on the Ox axis.</p>
     *
     * <p>The new image will represent the second image
     * concatenated with the first image.</p>
     *
     * @param image1 first image to concatenate to.
     * @param image2 second image to concatenate from.
     * @return a new Image representing the concatenation of the two images.
     */
    def horizontalConcat(image1: Image, image2: Image): Image =
        if image1.length != image2.length then Nil
        else image1.zip(image2).map(pixelZip => pixelZip(0) ++ pixelZip(1))

    /**
     * <p>Computes the transposed Image.</p>
     *
     * @param image original Image.
     * @return transposed Image of the original image.
     */
    private def transposeImage(image: Image): Image =
        image.head.indices
            .map(col =>
                image.indices
                    .map(row => image(row)(col))
                    .toList
            )
            .toList

    /**
     * <p>Rotates an Image AntiClockwise by a desired degree.</p>
     *
     * <p> (0; 90] -> rotation with 90 degrees.<br>
     *     (90; 180] -> rotation with 180 degrees.<br>
     *     (180; 270] -> rotation with 270 degrees.<br>
     *     (270; 360] -> rotation with 360 degrees.<br>
     * </p>
     *
     * <p>Function supports 2*k*pi evaluation of the angle.</p>
     *
     * @param image original Image to rotate.
     * @param degrees angle degree to rotate AntiClockwise.
     * @return a new Image rotated by the desired degrees.
     */
    def rotate(image: Image, degrees: Integer): Image =
        (0 until ((degrees % 360) / 90))
            .foldLeft(image)((acc, _) => transposeImage(acc.map(_.reverse)))

    private val gaussianBlurKernel: GrayscaleImage = List[List[Double]](
        List( 1, 4, 7, 4, 1),
        List( 4,16,26,16, 4),
        List( 7,26,41,26, 7),
        List( 4,16,26,16, 4),
        List( 1, 4, 7, 4, 1)
    ).map(_.map(_ / 273))

    private val Gx : GrayscaleImage = List(
        List(-1, 0, 1),
        List(-2, 0, 2),
        List(-1, 0, 1)
    )

    private val Gy : GrayscaleImage = List(
        List( 1, 2, 1),
        List( 0, 0, 0),
        List(-1,-2,-1)
    )

    /**
     * <p>Converts a rgb Image to a GrayScale Image.<p>
     *
     * @param image original Image to be transformed.
     * @return a new GrayScale Image.
     */
    private def imageToGrayscale(image: Image): GrayscaleImage =
        image.map(_.map(Util.toGrayScale))

    /**
     * <p>Applies the convolution operator on a GrayScale Image and
     * a kernel matrix.</p>
     *
     * @param image grayscale Image to compute the convolution.
     * @param kernel the matrix to apply the convolution operator.
     * @return a new GrayScale Image representing the convoluted image.
     */
    private def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage) : GrayscaleImage =
        val flattedKernel = kernel.flatten

        Util.getNeighbors(image, kernel.length / 2)
            .map(
                _.map(
                    _.flatten
                        .zip(flattedKernel)
                        .map(pair => pair(0) * pair(1))
                        .sum
                )
            )

    /**
     * <p>Generates the Image containing the edges of the original Image.</p>
     *
     * @param image original Image to compute the edges.
     * @param threshold the Limit in order to set Pixels white or black.
     * @return a new Image containing the processed image with only the edges.
     */
    def edgeDetection(image: Image, threshold : Double): Image =
        val gaussianBlurImage = applyConvolution(imageToGrayscale(image), gaussianBlurKernel)

        applyConvolution(gaussianBlurImage, Gx)
            .zip(applyConvolution(gaussianBlurImage, Gy))
            .map(rowZip => rowZip(0)
                .zip(rowZip(1))
                .map(elZip =>
                    if Math.abs(elZip(0)) + Math.abs(elZip(1)) <= threshold then Pixel(0, 0, 0)
                    else Pixel(255, 255, 255)
                )
            )

    /**
     * <p>Computes the modulo pascal triangle and codifies it in an Image.</p>
     *
     * @param M modulo number applied to the values.
     * @param func function to map an Integer to a Pixel Value.
     * @param size the size of the Pascal Triangle.
     * @return Pascal Triangle codified in an PPM Image.
     */
    def moduloPascal(M: Integer, func: Integer => Pixel, size: Integer): Image =
        (1 until size)
            .foldLeft(List(1 :: List.fill(size - 1)(0)))((acc, line) =>
                acc :+ ((0 to line).map(idxEl => {
                    if idxEl == line || idxEl == 0 then 1
                    else (acc(line - 1)(idxEl - 1) + acc(line - 1)(idxEl)) % M
                }).toList ++ List.fill(size - line - 1)(0))
            )
            .zip(1 to size)
            .map(lineZip =>
                val (leftLine, rightLine) = lineZip(0).splitAt(lineZip(1))
                leftLine.map(el => func(el)) ++ rightLine.map(_ => Pixel(0, 0, 0))
            )
}
