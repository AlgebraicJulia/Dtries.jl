using Test, AssociatedTests
import Base: tail

abstract type AbstractPath end

struct Path <: AbstractPath
    segments::Vector{Symbol}
end

function Base.length(p::Path)
    length(p.segments)
end

function Base.:(==)(p1::Path, p2::Path)
    p1.segments == p2.segments
end

Base.isempty(p::Path) = isempty(p.segments)

head(p::Path) = p.segments[1]

tail(p::Path) =
    length(p) > 0 ? Subpath(p, 2) : error("tried to take the tail of an empty path")

function segments(p::Path)
    p.segments
end

struct Subpath <: AbstractPath
    path::Path
    starting_from::Int
end

function Base.length(p::Subpath)
    length(p.path) - p.starting_from + 1
end

@tests Tuple{Subpath, length} begin
    @test length(tail(Path([:a, :b, :c]))) == 2
end

Base.isempty(p::Subpath) = p.starting_from > length(p.path)

head(p::Subpath) = p.path.segments[p.starting_from]

tail(p::Subpath) =
    length(p) > 0 ? Subpath(p.path, p.starting_from + 1) : error("tried to take the tail of an empty path")

function segments(p::Subpath)
    p.path.segments[p.starting_from:end]
end

function Base.:(*)(p1::AbstractPath, p2::AbstractPath)
    Path([segments(p1); segments(p2)])
end
