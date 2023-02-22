(** Clipping types for boolean operations. See Clipper2's docs for
       {{:http://www.angusj.com/clipper2/Docs/Units/Clipper/Types/ClipType.htm}
       visual demonstrations}. *)
type clip_type =
  [ `None
  | `Intersection (** AND -- regions covered by both subject and clip polygons *)
  | `Union
    (** OR -- regions covered by subject or clip polygons, or both
                   polygons *)
  | `Difference (** NOT -- regions covered by subject, but not clip polygons *)
  | `Xor
    (** exclusive or -- regions covered by subject or clip polygons, but
                 not both *)
  ]

(** Filling rules used by the clipping algorithm for boolean operations. See
       Clipper2's docs for a detailed
       {{:http://www.angusj.com/clipper2/Docs/Units/Clipper/Types/FillRule.htm}
       explanation} of how they differ). *)
type fill_rule =
  [ `EvenOdd (** only odd numbered sub-regions are filled *)
  | `NonZero (** non-zero sub-regions are filled *)
  | `Positive (** only sub-regions with winding counts [> 0] are filled *)
  | `Negative (** only sub-regions with winding counts [< 0] are filled *)
  ]

(** Defines the treatment of corners when offsetting paths. Visual examples
       are available in the Clipper2
       {{:http://www.angusj.com/clipper2/Docs/Units/Clipper/Types/JoinType.htm}
       docs}. *)
type join_type =
  [ `Square
    (** squaring applied uniformally at all joins where the {i internal}
             join angle is less than 90 degrees. The squared edg will be at
             exactly the offset distance from the join vertex *)
  | `Round
    (** rounding is appliedto all joins that have convex external
             angles, and it maintains the exact offset distance from the join vertex *)
  | `Miter
    (** there's a necessary limit to mitered joins (to avoid narrow angled
              joins producing excessively long and narrow
              {{:http://www.angusj.com/clipper2/Docs/Units/Clipper.Offset/Classes/ClipperOffset/Properties/MiterLimit.htm}
              spikes})). The limit sets the maximum distance in multiples of
              the [delta] specified for the offsetting operation (default is
              [2.], which is the minimum allowed). *)
  ]

(** Sets whether paths are treated as closed ([`Polygon]) when offsetting or
       open (and how to do so, if so). Visual examples are available in the Clipper2
      {{:http://www.angusj.com/clipper2/Docs/Units/Clipper/Types/EndType.htm}
      docs}. *)
type end_type =
  [ `Polygon (** paths are assumed to be closed and treated as polygons *)
  | `Joined (** ends are joined and the paths are treated as polylines *)
  | `Butt (** ends are squared off without any extrusion *)
  | `Square (** ends extend the offset amount while being {i squared off} *)
  | `Round (** ends extend the offset amount while being {i rounded off} *)
  ]
