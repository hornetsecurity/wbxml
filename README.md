# Haskell WAP Binary XML Library

This library provides data structures and functions to represent,
read, and write WAP Binary XML documents.

Data structures to represent WAP Binary XML documents are defined in
`Data.Wbxml.Types`, with functions for parsing and printing in
`Data.Wbxml.Parser` and `Data.Wbxml.Printer` respectively.  The module
`Data.Wbxml.Xml` provides conversion functions between WAP Binary XML
and XML, using the data types provided by the **xml-types** library.
Well-known WAP Binary XML document types are defined in
`Data.Wbxml.DocumentType`.

If your're interested in an exciting career with Haskell have a look at our career page:

https://www.hornetsecurity.com/de/karriere



## Usage

### Parsing

```haskell
import Data.ByteString (ByteString)
import Data.Wbxml.Types (Document)
import Data.Wbxml.Parser (wbxmlDocument)

import qualified Data.Attoparsec.ByteString as AP

parse :: ByteString -> Either String Document
parse input = <$> AP.parseOnly (wbxmlDocument <* AP.endOfInput) input
```

### Printing

```haskell
import Data.ByteString (ByteString)
import Data.Wbxml.Types (Document)
import Data.Wbxml.Printer (wbxmlDocument)

import qualified Data.ByteString.Builder as B

print :: Document -> ByteString
print doc = B.toLazyByteString (wbxmlDocument doc)
```

### XML Conversion

```haskell
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Wbxml.Types (Document, Header(..), Version(..), PublicId(..), Charset(..))
import Data.Wbxml.DocumentType (fromSpec, specWml13)
import Data.Wbxml.Xml (decodeDocument, encodeDocument)

import Data.Xml.Types as XML

decodeWml :: Document -> Either String XML.Document
decodeWml = decodeDocument doctype decodeText
    where doctype = fromSpec specWml13
          decodeText = return . decodeUtf8

encodeWml :: XML.Document -> Either String Document
encodeWml = encodeDocument doctype encodeText header
    where doctype = fromSpec specWml13
          encodeText = return . encodeUtf8
          header = Header (Version 1 0) (KnownPublicId 1) (Charset 1)
```
