
# Network Address Representations

This library provides types to represent network addresses of various
kinds (e.g., IPv4, IPv6) as well as functions to print those values in
their canonical external format, and parse address values.

## Address Representation

 - *Type* `ipv4-address`
 
   Instances of this type represent IPv4 network addresses. The canonical
   representation are four decimal octet values separated by dots `x.x.x.x`.
   The CPL is `ipv4-address`, `structure-object`, `t`.
   
 - *Type* `ipv6-address`

   Instances of this type represent IPv6 network addresses. The canonical
   representation are 8 limbs of up to four hexadecimal digits, separated by
   colons: `x:x:x:x:x:x:x:x` (and subject to further canonicalization rules.)
   The CPL is `ipv6-address`, `structure-object`, `t`.

 - *Type* `host-name`

   Instances of this type represent host names. Basically, an instance of
   this class is a structure wrapper for a string of subtype `host-name-string`.
   This type exists primarily for type discrimination purposes such as 
   generic function dispatch. The CPL is `host-name`, `structure-object`, `t`.
 
 - *Generic Function* `ipv4-address` _value_ &rarr; _address_
 
   Ensures, that _value_ is an instance of type `ipv4-address` coercing it
   when necessary (and possible.) If _value_ is neither an IPv4 address value
   nor can it be coerced, this function signals a `type-error` condition.
   
   This function always returns an instance of type `ipv4-address` or fails
   with an appropriate error condition. It never returns more than a single
   value. Applications may add their own methods to this function provided
   that they obey these restrictions.
   
   The following methods are pre-defined by the library
   
   - *Method* `ipv4-address` (_value_ `ipv4-address`) &rarr; _address_
   
     Answers _value_ unchanged.
   
   - *Method* `ipv4-address` (_value_ `string`) &rarr; _address_
   
     Answers the result of parsing _value_ via `parse-ipv4-address`. If the
     value cannot be parsed, signals a condition.
   
   - *Method* `ipv4-address` (_value_ `array`) &rarr; _address_
   
     This method works only if _value_ is an array matching the type
     specifier `(array (unsigned-byte 8) (4))`. Fails for all other types
     of arrays.
   
   - *Method* `ipv4-address` (_value_ `integer`) &rarr; _address_
   
     This method works only if _value_ is an integer of type `(unsigned-byte 32)`
     and it fails for all other values.
   
   - *Method* `ipv4-address` (_value_ `ipv6-address`) &rarr; _address_
   
     This method works only if _value_ is an address which consists only
     zero limbs with the exception of the last two limbs. The resulting 
     IPv4 address is constructed by using the value of the last two limbs
     of the input address.
 
 - *Function* `ipv4-address-p` _value_ &rarr; _boolean_
 
   Answers true, if _value_ is an instance of class `ipv4-address`.
 
 - *Generic Function* `ipv6-address` _value_ &rarr; _address_

   Ensures, that _value_ is an instance of type `ipv6-address` coercing it
   when necessary (and possible.) If _value_ is neither an IPv6 address value
   nor can it be coerced, this function signals a `type-error` condition.
   
   This function always returns an instance of type `ipv6-address` or fails
   with an appropriate error condition. It never returns more than a single
   value. Applications may add their own methods to this function provided
   that they obey these restrictions.

   The following methods are pre-defined by the library
   
   - *Method* `ipv6-address` (_value_ `ipv6-address`) &rarr; _address_
   
     Answers _value_ unchanged.
   
   - *Method* `ipv6-address` (_value_ `string`) &rarr; _address_
   
     Answers the result of parsing _value_ via `parse-ipv6-address`. If the
     value cannot be parsed, signals a condition.
   
   - *Method* `ipv6-address` (_value_ `array`) &rarr; _address_
   
     This method works only if _array_ is an array matching the type
     specifier `(array (unsigned-byte 8) (16))`. Fails for all other types
     of arrays.
   
   - *Method* `ipv6-address` (_value_ `integer`) &rarr; _address_
   
     This method works only if _value_ is an integer of type `(unsigned-byte 128)`
     and it fails for all other values.
   
   - *Method* `ipv6-address` (_value_ `ipv4-address`) &rarr; _address_
   
     Answers an IPv6 address constructed by embedding the given IPv4 address
     into the last two limbs (and keeping all other limbs 0.)

 - *Function* `ipv6-address-p` _value_ &rarr; _boolean_
 
   Answers true, if _value_ is an instance of class `ipv6-address`.
   
 - *Generic Function* `host-name` _value_ &rarr; _name_
 
 - *Function* `host-name-p` _value_ &rarr; _boolean_
   
## Auxiliary Types

 - *Type* `host-name-string`

   A subtype of `string` that includes only valid host names. Instances of
   this type have a maximum length of 253 characters, are composed only of
   letters, digits, hyphens, and periods, and must obey certain additional
   syntactic restrictions. Basically, they must match the regular expression
   `[a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?(\.[a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?)*\.?`
   
- *Function* `host-name-string-p` _value_ &rarr; _boolean_

  Tests, whether its argument is a syntactically well-formed host name 
  string.
  
- *Generic Function* `host-name-string` _value_ &rarr; _result_
  
  Ensures, that _value_ is an instance of type `host-name-string` coercing it
  when necessary (and possible.) If _value_ is neither a host name string nor 
  can it be coerced, this function signals a `type-error` condition.
   
  This function always returns an string or fails with an appropriate error 
  condition. It never returns more than a single value. Applications may add 
  their own methods to this function provided that they obey these restrictions.
  The pre-defined methods canonicalize their result to all lower-case.
  
  Pre-defined methods are:
  
  - *Method* `host-name-string` (_value_ `string`)
  
    Checks the given string for host-name-ness and returns an all-lower-case
    copy if the test succeeds.
  
  - *Method* `host-name-string` (_value_ `symbol`)
  
    Uses the `symbol-name` of _value_, and checks it for host-name-ness.
  
  - *Method* `host-name-string` (_value_ `character`)
  
    Treats _value_ as string of one character, and checks it for host-name-ness.

  - *Method* `host-name-string` (_value `t`)

    Signals a condition of type `type-error`.

## Comparing Addresses And Hashing

The following functions are provided to compare addresses (either for 
equivalence or for order.) In general, comparisons are only supported if 
both input arguments are of the same concrete address type, i.e., it is
not supported to compare an instance of `ipv4-address` to an instance
of `host-name` for order or equivalence.

All address implementations should provide support for `address-equal`
and `address-hash`. If possible, they should also provide `address-order`,
but that's optional. All pre-defined address types support ordering.

 - *Generic Function* `address-order` _value1_ _value2_ &rarr; _integer_
 
   Compares the input arguments _value1_ and _value2_ for order, and returns
   an integer encoding the result. The value of _integer_ is
   
    - `< 0` if _value1_ is considered to be strictly less than _value2_
    - `= 0` if _value1_ is considered to be strictly equal to _value2_
    - `> 0` if _value1_ is considered to be strictly greater than _value2_
    
   Client code usually does not invoke the function directly (though it may
   add new methods for new kinds of addresses.) Instead, this function is
   called by any of the following functions defined in this library:

    - *Function* `address=` _value1_ _value2_ &rarr; _boolean_
    - *Function* `address<` _value1_ _value2_ &rarr; _boolean_
    - *Function* `address>` _value1_ _value2_ &rarr; _boolean_
    - *Function* `address<=` _value1_ _value2_ &rarr; _boolean_
    - *Function* `address>=` _value1_ _value2_ &rarr; _boolean_
    - *Function* `address/=` _value1_ _value2_ &rarr; _boolean_
 
 - *Generic Function* `address-equal` _value1_ _value2_ &rarr; _boolean_
 
   Tests, whether _value1_ and _value2_ are equal, i.e., represent the same
   network address. For types that can also be ordered, the implementation of
   this function should be compatible with the ordering, i.e., this function
   should returns true for its input arguments if (and only if) `address-order`
   returns 0 when called with these arguments.
 
 - *Generic Function* `address-hash` _address_ &rarr; _integer_
 
   Computes a hash value for the given address. This function is intended 
   to be used with custom hash table implementations (such as `darts.lib.hastrie`)
   and is useful in Lisps that support custom hash functions in their hash table 
   implementations. Use `address-equal` as the equivalence test.
 
## Printing And Parsing

 - *Condition* `address-parse-error`
 
   An instance of this condition type is signalled if the `parse-xxx` parsing
   functions provided by this library cannot parse the input string provided. 
   The class precedence list is `address-parse-error`, `parse-error`, `error`, 
   `serious-condition`, `condition`, `t`.
 
 - *Generic Function* `address-parse-error-input` _object_ &rarr; _result_
 
   Answers an object describing the input of the failed parse operation. The
   value is a list of the form `(STRING :start INDEX :end INDEX)`
 
 - *Generic Function* `address-parse-error-expected-type` _object_ &rarr; _result_
 
   Answers the type of address the failed parse operation tried to extract
   from the input.

 - *Generic Function* `print-address` _value_ _stream_ `&key` &rarr; _undefined_
 
   Prints a representation of _value_ into _stream_. For addresses which can
   be fully represented as strings, there is the expectation, that unless 
   additional options are supplied via keyword arguments, the printed result
   can later be parsed back into an object equivalent to _value_.
   
 - *Function* `parse-ipv4-address` _string_ `&key` _start_ _end_ _junk-allowed_ &rarr; _address_
 
 - *Function* `parse-ipv6-address` _string_ `&key` _start_ _end_ _junk-allowed_ &rarr; _address_
 
 - *Function* `address-string` _address_ `&rest` _options_ &rarr; _string_
 
   Answers a string representation of _address_. Simply invokes `print-address` 
   on _address_, passing a temporary string stream as well as all additional
   `options`. Returns the generated string.

## Other Operations

 - *Generic Function* `address-bytes` _value_ &rarr; _array_

   Answers a byte array (i.e., array of sub-type `(array (unsigned-byte 8) (*))`)
   representation of the given address _value_. Not all address types support
   this function.
   
   The result should be a suitable first argument to `usocket`'s `socket-connect`
   function. A default method is provided, which returns `nil` for any input
   value.
