module CircularBuffer

type CircularBuffer<'a> = { Data: 'a list; Size: int }

let mkCircularBuffer size = { Data = []; Size = size }

let read buffer =

    if Seq.length buffer.Data = 0
    then failwith "NoNo"
    else
        (List.head buffer.Data,
         { buffer with
               Data = List.tail buffer.Data })

let write value buffer =

    if Seq.length buffer.Data = buffer.Size
    then failwith "NoNo"
    else
        let data = buffer.Data @ [ value ]
        { buffer with Data = data }

let clear buffer = mkCircularBuffer buffer.Size

let forceWrite value buffer =

    if Seq.length buffer.Data < buffer.Size
    then write value buffer
    else read buffer |> snd |> write value
