using System;
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Reflection;

namespace Paket.Bootstrapper
{
    internal static class BootstrapperHelper
    {
        const string PaketBootstrapperUserAgent = "Paket.Bootstrapper";

        internal static string GetLocalFileVersion(string target)
        {
            var localVersion = "";

            if (File.Exists(target))
            {
                try
                {
                    FileVersionInfo fvi = FileVersionInfo.GetVersionInfo(target);
                    if (fvi.FileVersion != null)
                        localVersion = fvi.FileVersion;
                }
                catch (Exception)
                {
                }
            }
            return localVersion;
        }

        internal static void PrepareWebClient(WebClient client, string url)
        {
            client.Headers.Add("user-agent", PaketBootstrapperUserAgent);
            client.UseDefaultCredentials = true;
            client.Proxy = GetDefaultWebProxyFor(url);
        }

        internal static HttpWebRequest PrepareWebRequest(string url)
        {
            var request = (HttpWebRequest)HttpWebRequest.Create(url);
            request.UserAgent = PaketBootstrapperUserAgent;
            request.UseDefaultCredentials = true;
            request.Proxy = GetDefaultWebProxyFor(url);
            request.AutomaticDecompression = DecompressionMethods.GZip | DecompressionMethods.Deflate;
            return request;
        }

        internal static void WriteConsoleError(string message)
        {
            WriteConsole(message, ConsoleColor.Red);
        }

        internal static void WriteConsoleInfo(string message)
        {
            WriteConsole(message, ConsoleColor.Yellow);
        }

        private static void WriteConsole(string message, ConsoleColor consoleColor)
        {
            var oldColor = Console.ForegroundColor;
            Console.ForegroundColor = consoleColor;
            Console.WriteLine(message);
            Console.ForegroundColor = oldColor;
        }

        internal static IWebProxy GetDefaultWebProxyFor(String url)
        {
            IWebProxy result = WebRequest.GetSystemWebProxy();
            Uri uri = new Uri(url);
            Uri address = result.GetProxy(uri);

            if (address == uri)
                return null;

            return new WebProxy(address)
            {
                Credentials = CredentialCache.DefaultCredentials,
                BypassProxyOnLocal = true
            };
        }

        internal static bool IsLatestVersion(string latestVersion, bool silent)
        {
            var exePath = GetExePath();
            var localVersion = GetLocalFileVersion(exePath);
            if (localVersion.StartsWith(latestVersion))
            {
                if (!silent)
                    Console.WriteLine("Bootstrapper is up to date. Nothing to do.");
                return true;
            }
            return false;
        }

        internal static string GetExePath()
        {
            return Assembly.GetExecutingAssembly().Location;
        }

        internal static void FileMove(string oldPath, string newPath)
        {
            try
            {
                if (File.Exists(newPath))
                {
                    File.Delete(newPath);
                }
            }
            catch (FileNotFoundException)
            {

            }

            File.Move(oldPath, newPath);
        }

        internal static void FileSwap(string oldFilePath, string newFilePath, bool silent)
        {
            var randomPath = Path.GetTempFileName();
            try
            {
                FileMove(oldFilePath, randomPath);
                FileMove(newFilePath, oldFilePath);
                if (!silent)
                    Console.WriteLine("Successfully swapped the old file for the new file.");
            }
            catch (Exception)
            {
                if (!silent)
                    Console.WriteLine("File swap failed. Resetting to the previous version.");
                FileMove(randomPath, oldFilePath);
                throw;
            }
        }
    }
}
