%title: Ludwig - Creating an approachable Haskell DSL
%author: Jasper Van der Jeugt
%date: 24-09-2016

# Overview

## About Fugue

I work at Fugue.  Fugue is a product that allows you manage to cloud resources
in a code-as-configuration way.

After you declare you configuration (in our DSL), Fugue sets up, and then
continuously enforces and monitors your infrastructure.  Currently, it is
available for AWS.

<http://fugue.co>

---

# Overview

## About Ludwig

Ludwig is the DSL in which users declare their configuration.  It is inspired by
languages like Haskell and Ocaml, but also by YAML and JSON.

The compiler, `lwc`, is written in Haskell.

---

# Why not YAML?

## Ansible example

Well,

    command: echo {{ item }}
    with_items: [0, 2, 4]
    when: item > 2

---

# Why not YAML?

## Problems

- How do you document YAML files?
- Where do you store configurations?
- How do you provide error messages?

---

# Why not YAML?

## Advantaged of a "real" language

- Module system
- Documentation generator
- Type system

---

# A typed language

## Simple example

A classic Virtual Private Cloud:

    my-vpc:
        region: Us-east-1
        cidr: "10.0.0.0/16"

---

# A typed language

## Simple example

With explicity type annotation:

    {region: Region, cidr: String} my-vpc:
        region: Us-east-1
        cidr: "10.0.0.0/16"

---

# A typed language

## Simple example

Let's try another region:

    {region: Region, cidr: String} my-vpc:
        region: Us-east-3
        cidr: "10.0.0.0/16"

---

# A typed language

## Simple example

    $ lwc test.lw
    ludwig (scope error):
      "test.lw" (line 4, column 13):
      Not in scope:
      
        4|     region: Us-east-3
                       ^^^^^^^^^
      
      Constructor not in scope: Us-east-3
      Hint: perhaps you mean the constructor
      Us-east-1 (from Fugue.Core.AWS.Common)

---

# The type system

## Scoped labels

Initially based on "scoped labels" by Daan Leijen, 2005

- Very simple rules with good type inference
- Allows extending and updating fields

---

# The type system

## Scoped labels: disadvantages

    my-vpc:
        region: Us-east-1
        cidr: "10.0.0.0/16"
    
    # New field
    my-vpc-1: {tag = "prod" | my-vpc}
    
    # ALSO new field!
    my-vpc-2: {region = Us-west-1 | my-vpc}
    
    # Update field
    my-vpc-3: {region := Us-west-1 | my-vpc}

---

# The type system

## Scoped labels: disadvantages

From personal experience: it is very, very hard to explain why this is a good
idea to someone who has had little exposure to typed programming languages.

Alternative solution: _extend = update_ (field shadowing).

---

# The type system

## Field shadowing

    my-vpc:
        region: Us-east-1
        cidr: "10.0.0.0/16"
    
    my-vpc-1: my-vpc with {tag: "prod"}
    
    # Previous `region` field is "shadowed"
    my-vpc-2: my-vpc with {region: Us-west-1}

Still able to infer principal types!

---

# The type system

## Records

Records are used a lot so we have some syntactic sugar for updates.

    my-network: network with
        vpc.cidr: "10.0.0.0/16"
        igw.InternetGateway.tag: "dev"

Desugars to simple record updates (with `case` statements for constructors).

---

# Patterns

Records are not enough.  The AWS API is **huge**, but a lot of use cases are
very, very similar.  We need a magical tool that enables code reuse.

    exampleNetwork: network {
      name: "Example VPC",
      region: exampleRegion,
      cidr: "10.0.0.0/16",
      publicSubnets: [
        (examplePrimaryAz, "10.0.1.0/24"),
        (exampleSecondaryAz, "10.0.2.0/24")
      ],
      privateSubnets: []
    }

---

# Functions

    fun vpc(cidr: String) -> Vpc:
        region: my-default-region
        cidr: cidr
    
    vpc-a: vpc("10.0.0.0/16")
    vpc-b: vpc("192.168.100.0/22")

---

# Functions

Unfortunately the AWS API is a huge beast in many ways:

    fun launch-configuration(
        keyName: Optional<String>
        securityGroups: List<SecurityGroup>
        userData: Optional<String>
        instanceType: InstanceType
        blockDeviceMappings: Optional<List<InstanceBlockDeviceMapping>>
        instanceMonitoring: Optional<Bool>
        ...
      ) -> LaunchConfiguration:
        ...

---

# Functions

And code like this is not very nice to read:

    lca: launch-configuration(
        Optional("zDebugKey"), None, None, M3_Large, None,
        ...
      )

---

# Named parameters

This is slightly better:

    lca: launch-configuration(
        keyName: Optional("zDebugKey"),
        securityGroups: None,
        userData: None,
        instanceType: M3_Large,
        blockDeviceMappings: None,
        ...
      )

How do we add named parameters to a Haskell-like language?

---

# Idea: named parameters through records

A function is named parameters is simply a unary function.
The only argument is a record.

    lca: launch-configuration{
        keyName: Optional("zDebugKey"),
        securityGroups: None,
        userData: None,
        instanceType: M3_Large,
        blockDeviceMappings: None,
        ...
      }

---

# Idea: named parameters through records

Bonus: this allows you to do some nice "1st-class" things with arguments.

    large-launch-configuration: fun(args):
      launch-configuration(
        args with {instanceType: M3_Large}
      )

Type inference works well here: the function now takes the same "arguments" as
`launch-configuration` minus the `instanceType` field.

---

# Named parameters: None padding

This is very verbose:

    lca: launch-configuration{
        keyName: Optional("zDebugKey"),
        securityGroups: None,
        userData: None,
        instanceType: M3_Large,
        blockDeviceMappings: None,
        ...
      }

---

# Named parameters: None padding

    lca: launch-configuration{
        keyName: Optional("zDebugKey"),
        instanceType: M3_Large
      }


Syntax-directed coercions

- Typechecker: We allow padding record types with `Optional<a>` fields
  **in arguments of function applications**.

- Interpreter: missing fields are `None`.

---

# Named parameters: Optional lifting

    lca: launch-configuration{
        keyName: Optional("zDebugKey"),
        instanceType: M3_Large
      }

One issue remaining: try explaining to a system administrator who has only seen
bash and Python why the `Optional()` call is necessary here.

---

# Named parameters: Optional lifting

    lca: launch-configuration{
        keyName: "zDebugKey",
        instanceType: M3_Large
      }

Similar syntax-based coercion: we allow converting `a` to `Optional<a>` on the
right hand side of a record construction or record update.

`Optional` has (fairly minimal) special treatment in the compiler but the
advantages are worth it.

---

    
         ____________ 
        < Questions? >
         ------------ 
                \   ^__^
                 \  (oo)\_______
                    (__)\       )\/\
                        ||----w |
                        ||     ||
    
