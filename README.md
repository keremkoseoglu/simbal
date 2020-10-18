# Simbal

This ABAP project provides a simplified utility class for application log creation. 

## Installation

You can install Simbal to your SAP system using [abapGit](https://github.com/abapGit/abapGit).

## Usage

Before using Simbal, you need to have an Application Log object definition in SLG0.

Creating a new instance:

```
DATA(simbal) = NEW ycl_simbal(
   object    = 'MY_SLG0_OBJECT'
   subobject = 'MY_SLG0_SUBOBJECT' ).
```

You can add messages using **add_** methods provided in **YCL_SIMBAL**.

You can access message data using **get_** methods provided in **YCL_SIMBAL**.

To save messages to the application log:

```
simbal->save_to_db( ).
```

To show the log contents to the user over SAP GUI, you may use any method within **YCL_SIMBAL_GUI**.
