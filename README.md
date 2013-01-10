# Enum Paradise (WORK IN PROGRESS)

Scala enumeration implementation using type macros provided by [Macro Paradise](http://docs.scala-lang.org/overviews/macros/paradise.html)

## Usage

    object Days extends Enum('Monday, 'Tuesday, 'Wednesday, 'Thursday, 'Friday)

    type X = Days.Value

    def xs: Seq[Days.Value] = Days.values

    import Days._
    def f(x: Value) = x match {
      case Monday => 
      case Tuesday => 
      case Wednesday => 
      case Thursday => 
      case Friday => 
    }

    def f(x: Enumerable) = x.values
    f(Days)

## License

    This software is licensed under the Apache 2 license, quoted below.

    Copyright 2009-2012 Alois Cochard 

    Licensed under the Apache License, Version 2.0 (the "License"); you may not
    use this file except in compliance with the License. You may obtain a copy of
    the License at http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
    WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
    License for the specific language governing permissions and limitations under
    the License.
