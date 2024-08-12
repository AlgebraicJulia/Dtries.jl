using Test, AssociatedTests
import Base: tail

abstract type AbstractPath end

struct Path <: AbstractPath
    segments::Vector{Symbol}
end
export Path

function Base.length(p::Path)
    length(p.segments)
end

function Base.:(==)(p1::Path, p2::Path)
    p1.segments == p2.segments
end

Base.isempty(p::Path) = isempty(p.segments)

head(p::Path) = p.segments[1]

tail(p::Path) = Subpath(p, 2)

struct Subpath <: AbstractPath
    path::Path
    starting_from::Int
end

Base.isempty(p::Subpath) = p.starting_from > length(p.path)

head(p::Subpath) = p.path.segments[p.starting_from]

tail(p::Subpath) = Subpath(p.path, p.starting_from + 1)
