module Paket.RemoteUpload

open System
open System.Globalization
open System.IO
open System.Net
open System.Text
open Paket
open Paket.Logging

type System.Net.WebClient with
    member x.UploadFileAsMultipart (url : Uri) filename = 
        let fileTemplate = 
            "--{0}\r\nContent-Disposition: form-data; name=\"{1}\"; filename=\"{2}\"\r\nContent-Type: {3}\r\n\r\n"
        let boundary = "---------------------------" + DateTime.Now.Ticks.ToString("x", CultureInfo.InvariantCulture)
        let fileInfo = (new FileInfo(Path.GetFullPath(filename)))
        let fileHeaderBytes = 
            String.Format
                (CultureInfo.InvariantCulture, fileTemplate, boundary, "package", "package", "application/octet-stream") 
            |> Encoding.UTF8.GetBytes
        let newlineBytes = Environment.NewLine |> Encoding.UTF8.GetBytes
        let trailerbytes = String.Format(CultureInfo.InvariantCulture, "--{0}--", boundary) |> Encoding.UTF8.GetBytes
        x.Headers.Add(HttpRequestHeader.ContentType, "multipart/form-data; boundary=" + boundary)
        use stream = x.OpenWrite(url, "PUT")
        stream.Write(fileHeaderBytes, 0, fileHeaderBytes.Length)
        use fileStream = File.OpenRead fileInfo.FullName
        fileStream.CopyTo(stream, (4 * 1024))
        stream.Write(newlineBytes, 0, newlineBytes.Length)
        stream.Write(trailerbytes, 0, trailerbytes.Length)
        ()


let GetUrlWithEndpoint (url: string option) (defaultUrl : string) (endPoint: string option) (defaultEndpoint :string) =
    let (|UrlWithEndpoint|_|) url = 
        match url with
        | Some url when not (String.IsNullOrEmpty(Uri(url).AbsolutePath.TrimStart('/'))) -> Some(Uri(url)) 
        | _                                                                              -> None  

    let (|IsUrl|_|) (url: string option) =
        match url with
        | Some url -> Uri(url.TrimEnd('/') + "/") |> Some
        | _        -> None
    
    let urlWithEndpoint = 
        match (url, endPoint) with
        | None                   , _                   -> Uri(Uri(defaultUrl), defaultEndpoint)
        | IsUrl baseUrl          , Some customEndpoint -> Uri(baseUrl, customEndpoint.TrimStart('/'))
        | UrlWithEndpoint baseUrl, _                   -> baseUrl
        | IsUrl baseUrl          , None                -> Uri(baseUrl, defaultEndpoint)
        | Some whyIsThisNeeded   , _                   -> failwith "Url and endpoint combination not supported"  
    urlWithEndpoint.ToString ()

  
let Push maxTrials nugetUrl symbolUrl apiKey (packageFileName : string) =
    let symbolPkg (nuget :string) = 
        nuget.Split([|".nupkg"|], StringSplitOptions.RemoveEmptyEntries).[0] + ".symbols.nupkg" 

    let rec push file url trial =
        tracefn "Pushing package %s to %s - trial %d" packageFileName nugetUrl trial
        try
            ignore()
            let client = Utils.createWebClient(url, None)
            client.Headers.Add("X-NuGet-ApiKey", apiKey)

            client.UploadFileAsMultipart (new Uri(url)) file
            |> ignore
            tracefn "Pushing %s complete." packageFileName
        with
        | exn when trial = 1 && exn.Message.Contains("(409)") ->
            failwithf "Package %s already exists." file
        | exn when trial < maxTrials ->            
            if exn.Message.Contains("(409)") |> not then // exclude conflicts
                traceWarnfn "Could not push %s: %s" file exn.Message
                push url file (trial + 1)

    push packageFileName nugetUrl 1 
    
    let symbolPackageName = packageFileName |> symbolPkg
    if File.Exists symbolPackageName then
        push symbolPackageName symbolUrl  1