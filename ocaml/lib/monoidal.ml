module type MonoidalCategory = sig
  type 'a obj
  type 'a mor
  val id : 'a obj -> 'a mor
  val compose : 'b mor -> 'a mor -> 'a obj -> 'b obj -> 'c mor
  val tensor : 'a obj -> 'b obj -> 'c obj
  val tensorMor : 'a mor -> 'b mor -> 'c mor
end

module MonoidalCategory : MonoidalCategory = struct
  type 'a obj = 'a
  type 'a mor = 'a -> 'a
  let id x = x
  let compose f g a b = f (g a)
  let tensor a b = (a, b)
  let tensorMor f g (a, b) = (f a, g b)
end