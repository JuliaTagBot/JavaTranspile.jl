module JavaTranspile

# @enum AccessLevelModifiers public private protected
# @enum Modifiers static final abstract synchronized volatile

# Java primitive types
# immutable byte value::Int8 end
# immutable short value::Int16 end
# immutable int value::Int32 end
# immutable long value::Int64 end
# immutable float value::Float32 end
# immutable double value::Float64 end
# immutable boolean value::Bool end
# immutable char value::Char end

# immutable String value::Base.String end

type Variable
    name::String
    annotations::Vector{String}
    modifiers::Vector{String}
    typ::String
    init::String
end

Variable() = Variable("", String[], String[], "", "")

function Base.show(io::IO, m::Variable)
    println(io, "Java.Variable: $(m.name)")
    println(io, "  annotations: $(join(m.annotations, ", "))")
    println(io, "   definition: $(join(m.modifiers, ' ')) $(m.typ) $(m.name)")
end

function Base.showcompact(io::IO, m::Variable)
    print(io, "$(join(m.modifiers, ' ')) $(m.typ) $(m.name)")
end

type Method
    name::String
    annotations::Vector{String}
    modifiers::Vector{String}
    returntype::String
    args::Vector{String}
    body::String
end

Method() = Method("", String[], String[], "", String[], "")

function Base.show(io::IO, m::Method)
    println(io, "Java.Method: $(m.name)")
    println(io, "  annotations: $(join(m.annotations, ", "))")
    println(io, "   definition: $(join(m.modifiers, ' ')) $(m.returntype == m.name ? "" : string(m.returntype, " "))$(m.name)($(join(m.args, ", ")))")
end

function Base.showcompact(io::IO, m::Method)
    print(io, "$(join(m.modifiers, ' ')) $(m.returntype == m.name ? "" : string(m.returntype, " "))$(m.name)($(join(m.args, ", ")))")
end

type Class
    name::String
    classtype::String
    package::String
    imports::Vector{String}
    annotations::Vector{String}
    classdef::String
    enums::Vector{String}
    variables::Vector{Variable}
    methods::Vector{Method}
    innerclasses::Vector{Class}
end

Class() = Class("", "", "", String[], String[], "", String[], Variable[], Method[], Class[])

function Base.show(io::IO, c::Class)
    println(io, "Java.Class: $(c.name)")
    println(io, "        package: $(chop(c.package))")
    println(io, "      # imports: $(length(c.imports))")
    println(io, "    annotations: $(join(c.annotations, ", "))")
    println(io, "     definition: $(chop(c.classdef))")
    if c.classtype == "enum"
    println(io, "          enums: ")
    for e in c.enums
        println(io, (" " ^ 16) * e)
    end
    end
    println(io, "         fields: ")
    buf = IOBuffer()
    for f in c.variables
        showcompact(buf, f)
        println(io, (" " ^ 16) * takebuf_string(buf))
    end
    println(io, "        methods: ")
    for m in c.methods
        showcompact(buf, m)
        println(io, (" " ^ 16) * takebuf_string(buf))
    end
    println(io, "  inner classes: ")
    for f in c.innerclasses
        println(io, (" " ^ 16) * f.name)
    end
end

function parseline(f)
    line = strip(readline(f))
    while !eof(f)
        if line == ""
            # if empty line, skip
            line = strip(readline(f))
        elseif startswith(line, "//")
            # if comment, skip line
            line = strip(readline(f))
        elseif startswith(line, "/*")
            # if multi-line comment, keep reading till end
            while !contains(line, "*/")
                line = strip(readline(f))
            end
            line = strip(readline(f))
        else
            break
        end
    end
    return line
end

function parsepackage(f, line, class)
    if startswith(line, "package")
        class.package = line
        line = parseline(f)
    end
    return line
end

function parseimports(f, line, class)
    while !eof(f)
        if startswith(line, "import")
            push!(class.imports, line)
            line = parseline(f)
        else
            break
        end
    end
    return line
end

function parseclassannotations(f, line, class)
    while !eof(f)
        if startswith(line, "@")
            if contains(line, "(") && !contains(line, ")")
                while !contains(line, ")")
                    line *= parseline(f)
                end
            end
            push!(class.annotations, line)
            line = parseline(f)
        else
            break
        end
    end
    return line
end

function parseclassdefinition(f, line, class)
    while !contains(line, "{")
        line *= string(" ", parseline(f))
    end
    class.classdef = line
    if contains(line, "class")
        # abstract or regular class
        if contains(line, "abstract")
            class.classtype = "abstract"
        else
            class.classtype = "class"
        end
    elseif contains(line, "interface")
        # interface class
        class.classtype = "interface"
    elseif contains(line, "enum")
        # enum class
        class.classtype = "enum"
        # parse comma-separated enum list until semicolon
        class.enums = String[strip(string(x)) for x in split(chop(parseuntil(f, ';')), ',')]
    else
        error("unsupported class type detected: $line")
    end
    return
end

function skipblock(f, bracecount)
    line = parseline(f)
    while true
        bracecount += count(c -> c == '{', line)
        bracecount -= count(c -> c == '}', line)
        bracecount <= 0 && break
        line = parseline(f)
    end
end

function nextline(f)
    ch = string(Char(Base.peek(f)))
    while ismatch(r"\s", ch)
        read(f, Char)
        eof(f) && break
        ch = string(Char(Base.peek(f)))
    end
end

function parseclassbody(f, line, class)
    annotations = String[]
    while !eof(f)
        nextline(f)
        token = parseuntil(f, 0, '<', '>', ' ', '(')
        if startswith(token, "//")
            parseline(f)
            continue
        elseif startswith(token, "/*")
            while !contains(token, "*/")
                token = parseline(f)
            end
            continue
        end
        if token == "}" || token == "};" || token == ""
            # end of class/file
            break
        elseif token[1] == '@'
            # method (or variable?) annotations
            token *= token[end] == '(' ? parseuntil(f, 1, '(', ')', ')') : ""
            push!(annotations, token)
        elseif token == "{"
            # initialization block
            skipblock(f, 1)
        elseif token == "static" && Char(Base.peek(f)) == '{'
            # static initialization block
            skipblock(f, 0)
        else
            # we're parsing an inner class, member variable, or method
            modifiers = String[]
            while token in ("public", "private", "protected", "static", "final", "abstract", "synchronized", "volatile")
                push!(modifiers, token)
                token = parseuntil(f, 0, '<', '>', ' ', '(')
            end
            # we're now positioned at the type (or method name for constructors)
            if token in ("class", "enum", "interface")
                # inner class
                ch = ' '
                while ch != '{'
                    ch = read(f, Char)
                end
                skipblock(f, 1)
                continue
            elseif token[end] == '('
                # class constructor
                name = token[1:end-1]
                returntype = name
            else
                # return type
                returntype = token
                # method or variable name
                token = parseuntil(f, 0, '<', '>', ' ', '(', ';')
                name = chop(token)
            end
            if token[end] == '('
                # we're parsing a method
                args = parseargs(chop(parseuntil(f, 1, '(', ')', ')')))
                body = string(read(f, Char))
                if body == ";"
                    body = ""
                else
                    body *= parseuntil(f, Int(body == "{"), '{', '}', '}')
                end
                push!(class.methods, Method(name, splice!(annotations, 1:length(annotations)), modifiers, returntype, args, body))
            else
                # we're parsing a variable
                init = token[end] == ';' ? "" : parseuntil(f, ';')
                push!(class.variables, Variable(name, splice!(annotations, 1:length(annotations)), modifiers, returntype, init))
            end
        end
    end
end

function parseuntil(io, bracecount, ob, cb, chars...)
    buf = IOBuffer()
    stripleadingwhitespace = true
    while !eof(io)
        ch = read(io, Char)
        if stripleadingwhitespace
            while ch == ' '
                ch = read(io, Char)
            end
            stripleadingwhitespace = false
        end
        write(buf, ch)
        if ch == ob
            bracecount += 1
        elseif ch == cb
            bracecount -= 1
        end
        detectclosing = false
        for c in chars
            ch == c && (detectclosing = true)
        end
        detectclosing && bracecount == 0 && break
    end
    return strip(takebuf_string(buf))
end
parseuntil(io, c) = parseuntil(io, 0, '<', '>', c)

function parseargs(args)
    length(args) <= 1 && return String[]
    return String[string(x) for x in split(args, ',')]
end

function parse(file)
    isfile(file) || throw(ArgumentError("'$file' is not a valid file"))
    class = Class()
    class.name = basename(file)[1:end-5]
    open(file) do f
        line = parseline(f)
        line = parsepackage(f, line, class)
        line = parseimports(f, line, class)
        line = parseclassannotations(f, line, class)
        parseclassdefinition(f, line, class)
        parseclassbody(f, line, class)
    end
    return class
end

function generateclient(class::Class, output_dir)

end

function generatetype(class::Class)

end

#TODO
 # JavaParser:
   # parse inner classes
   # setup proper test suite
   # return actual types in classes/methods?
   # don't show constructor type twice in method show?
 # write generateclient method
 # write generatetype method
 # rename this package to "JavaParser"
 # create "Java" submodule to hold type definitions, methods; export this submodule

# need:
 # transpile a regular class or enum to corresponding Julia type
 # transpile a "client" interface to corresponding Julia module + methods
 # Client generation
  # create a module for client interface w/ using Requests, JSON
  # create a method for each interface method
  # for each non primitive type:
   # find path from imports
   # parse custom class path
   # generate corresponding Julia type

end # module
