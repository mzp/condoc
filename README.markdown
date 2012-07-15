Coqdoc to Markdown
=================

DESCRIPTION
------------

Convert Coqdoc to Markdown format. This aim to convert Software Foundation( http://www.cis.upenn.edu/~bcpierce/sf/ or http://proofcafe.org/sf/ ) to Sphinx.

BUILD/RUN
------------------

## Build

    $ sbt package

## Run

    $ cd target/scala-2.9.1
    $ scala condoc_2.9.1-0.1-SNAPSHOT.jar Foo.v > Foo.markdown 

Tips
-------------------

To convert other formats (e.g. reStructured Text), use pandoc.

* * * * * * * * * *
## License

Copyright 2012- MIZUNO Hiroki(mzp)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

