class MyNumber:
    def __init__(self, num):
        self.num = num
    # We can override operators individually for user-defined classes
    # But we cannot do it on a large scale or for built-in types (not that I know of)
    def __add__(self, other):
        if isinstance(other, int):
            return MyNumber(self.num + other)
        if isinstance(other, MyNumber):
            return MyNumber(self.num + other.num)

# We can override built-in functions
# Because built-in frames have lower priority than global frames
len = lambda x: f"I call len on {x}"
print(len([10,20]))
# prints "I call len on [10, 20]"

# Is there a moral to the story?
# The moral of the story is that even though LISP gives people a lot of freedom, most people don't really need that level of freedom. In fact, they are afraid of the freedom - when I search how to overload built-in functions online, the first results are how to recover and why you should never do that. I personally never thought about extending default arithmetic, and couldn't find a use case for such feature except when I am implementing a new language.
# Any comments on why these different styles of programming emerged?
