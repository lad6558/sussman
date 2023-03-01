# The following code is copied verbatim from my 6.009 lab written for LA
# It is basically compose with multiple arguments
# One could use the * as spread operator to pass in multiple arguments
# And one could use *args to take in multiple arguments
# I assume they borrowed from lisp? I heard that Python is somewhat influenced by functional programming. To what extent is that true?
# Returning multiple values is easy, espeically in tuples and lists, but I don't know a way to return multiple *values*
def filter_cascade(filters):
    """
    Given a list of filters (implemented as functions on images), returns a new
    single filter such that applying that filter to an image produces the same
    output as applying each of the individual ones in turn.
    """
    if len(filters) == 0:
        return (lambda image: image)
    return (lambda image: filter_cascade(filters[1:])(filters[0](image)))
# It is funny that before I did 6905 I did this function in a for loop but now it is just lambda everywhere
# Functional programming... it takes an effect on you. And when you notice it is already too late.
