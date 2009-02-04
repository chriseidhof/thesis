\section {Introduction}

A lot of business systems these days are automated and made available online.
These systems typically contain complex workflows that enable the users of the
system to accomplish business-specific tasks. By modeling these workflows in
Haskell, a strongly typed, functional language we aim to provide powerful
building blocks to model these workflows and create working web applications.

We provide a powerful combinator library for:

* Modeling the domain
* Building web forms for user-generated input
* Chaining and combining user actions into workflows

Typically, web applications are built in a way that maps URLs to specific
actions. The workflow of web applications is very implicit: *goto alike*

TODO: references to iTasks
TODO: Voorbeeld forms in php/ruby
TODO: Voordelen types

\section{Comparison to other web programming languages}

\section{Generic Programming}

Because our framework primarily targets web applications that are backed by a
database, we provide a lot of generic functions for database-like records. These
records are a subset of the regular Haskell datatypes: they have only one
constructor, that contains named fields.

We also have a Rep datatype that describes the anatomy of a record:

> data Rep r where
>   RInt     :: Rep Int
>   RInteger :: Rep Integer
>   RString  :: Rep String
>   Field    :: String -> Rep r -> Rep r
>   (:*:)    :: Rep r1 -> Rep r2 -> Rep (r1,r2)

This is a lot like regular datatype-generic programming (todo: reference to
L\"oh, Jeuring and more). However, we don't support choice (sums) directly, as
this is not a feature of database systems. If we now define a typeclass
Representable that converts back and forth between a structural representation
and the datatype we can write functions for the structural representation:

> class Representable a r | a -> r where
>   to   :: a -> r
>   from :: r -> a
>   rep  :: a -> Rep r

The functional dependency makes sure that we can only give one structural
representation for a datatype. The structural representations can be
mechanically derived using Template Haskell, a manual instance for the |Person|
datatype looks like this:

> data User = User {name :: String, age :: Integer, email :: String, city :: String}
> 
> instance Representable User (String, (Integer, (String, String))) where
>   to (User a b c d)     = a & b & c & d
>   from (a, (b, (c, d))) = User a b c d
>   rep _ =  Field "name"  RString
>        :*: Field "age"   RInteger 
>        :*: Field "email" RString 
>        :*: Field "city"  RString

Now we have everything in place to define a generic html-view on datatypes. A
view is defined as a typeclass, and it takes an argument a and produces HTML for
that:

> class View a where
>   view :: a -> X.Html

Now we can we make every representable datatype also viewable:

> instance (Representable a b) => View a where
>   view x = repView (rep x) $ to x
>    
> repView :: Rep r -> r -> X.Html
> repView RInt             i = toHtml (show i)
> repView RInteger         i = toHtml (show i)
> repView RString          s = toHtml s
> repView (Field lbl r)    v = (X.label << (capitalize lbl ++ ": ")) +++ repView r v
> repView (r1 :*: r2) (l, r) = repView r1 l +++ X.br +++ repView r2 r

\section{Statically optimized}
