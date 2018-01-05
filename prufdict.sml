signature DICT =
  sig
    type (''k, 'v) dict
    exception DictError of string
    val empty : (''k, 'v) dict
    val of_raw : (''k * 'v) list -> (''k, 'v) dict
    val to_raw : (''k, 'v) dict -> (''k * 'v) list 
    val insert : ( 'k, 'v) dict ->  'k -> 'v -> ('k, 'v) dict
    val lookup : (''k, 'v) dict -> ''k -> 'v option
    val search : (''k, 'v) dict -> ''k -> 'v
    val member : (''k, 'v) dict -> ''k -> bool
  end

structure assoc :> DICT =
  struct
    type (''k, 'v) dict = (''k * 'v) list
    exception DictError of string

    val empty = []

    fun insert (d : ('k, 'v) dict) (k : 'k) (v : 'v) : ('k, 'v) dict =
        (k, v) :: d

    fun of_raw (d' : (''k * 'v) list) : (''k, 'v) dict = d'
    fun to_raw (d' : (''k, 'v) dict) : (''k * 'v) list = d'

    fun lookup (d : (''k, 'v) dict) (k : ''k) : 'v option =
        case d
         of [] => NONE
          | ((k', v') :: d') =>
            if k' = k then
              SOME v'
            else
              lookup d' k

    fun search (d : (''k, 'v) dict) (n : ''k) : 'v =
      let val v' = lookup d n in
        case v'
         of NONE => raise DictError "Key not found"
          | SOME v => v
      end

    fun member (d : (''k, 'v) dict) (n : ''k) : bool =
      let val v' = lookup d n in
        case v'
         of NONE   => false
          | SOME v => true
      end

  end
