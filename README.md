# Math-Parser

Header-only math parser. It's decently performative when compared to a library like `tinyexpr`, except it is simpler and consequently has a cleaner API. See sample.cpp for use.

Internally, the library is built around an explicit [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree) and a modular evaluation engine, making it easy to extend with custom functionality without entangling the parser logic.

Some possible edge cases may or may not exist, although I have tested quite extensively. 

Perhaps one day I will extend the functionality to include functions such as $$\text{inf}$$⁡ and $$\text{sup}$$⁡, as well as other operations like integration and common statistical or combinatorial functions—the likes of which are found in modern graphing calculators.
