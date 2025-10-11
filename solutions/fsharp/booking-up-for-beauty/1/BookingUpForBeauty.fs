module BookingUpForBeauty

open System
open System.Globalization

let schedule (appointmentDateDescription: string) : DateTime =

    let formats =
        [| "MM/dd/yyyy HH:mm:ss"
           "M/dd/yyyy HH:mm:ss"
           "MMMM dd, yyyy HH:mm:ss"
           "MMMM d, yyyy HH:mm:ss"
           "dddd, MMMM d, yyyy HH:mm:ss"
           "dddd, MMMM dd, yyyy HH:mm:ss" |]

    DateTime.ParseExact(
        appointmentDateDescription,
        formats,
        CultureInfo.InvariantCulture,
        DateTimeStyles.None)

let hasPassed (appointmentDate: DateTime) : bool = appointmentDate < DateTime.Now

let isAfternoonAppointment (appointmentDate: DateTime) : bool =
    appointmentDate.Hour >= 12
    && appointmentDate.Hour < 18

let description (appointmentDate: DateTime) : string =
    let data =
        appointmentDate.ToString("M/d/yyyy h:mm:ss tt")

    sprintf "You have an appointment on %s." data

let anniversaryDate () : DateTime =
    let year = DateTime.Now.Year
    new DateTime(year, 9, 15)
