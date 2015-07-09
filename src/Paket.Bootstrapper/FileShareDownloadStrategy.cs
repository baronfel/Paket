using System;
using System.IO;
using System.Linq;

namespace Paket.Bootstrapper
{
    internal class FileShareDownloadStrategy : IDownloadStrategy
    {
        string uncPath;

        public FileShareDownloadStrategy(string uncPath)
        {
            this.uncPath = uncPath;
        }

        public IDownloadStrategy FallbackStrategy
        {
            get;set;
        }

        public string Name
        {
            get { return "File"; }
        }

        public void DownloadVersion(string latestVersion, string target, bool silent)
        {
            var newVersionPath = Path.Combine(uncPath, string.Format("paket.{0}.exe", latestVersion));
            var tempPath = Path.GetTempFileName();
            File.Copy(newVersionPath, tempPath, true);

            BootstrapperHelper.FileSwap(target, tempPath, silent);
        }

        public string GetLatestVersion(bool ignorePrerelease)
        {
            var pakets = 
                Directory.GetFiles(uncPath, "paket.*.exe")
                .Where(x => !x.Contains("bootstrapper"))
                .Select(Path.GetFileName);

            var versions = 
                    pakets
                    .Select(x => x.Replace("paket.", "").Replace(".exe", ""))
                    .Distinct(StringComparer.OrdinalIgnoreCase);

            if (ignorePrerelease)
            {
                versions = versions.Where(x => !x.Contains("-"));
            }

            return versions.OrderBy(x => x).First();
        }

        public void SelfUpdate(string latestVersion, bool silent)
        {
            if (BootstrapperHelper.IsLatestVersion(latestVersion, silent))
                return;

            var exePath = BootstrapperHelper.GetExePath();
            var targetBootstrapper = Path.Combine(uncPath, string.Format("paket.bootstrapper.{0}.exe", latestVersion));
            var localPath = Path.GetTempFileName();
            BootstrapperHelper.FileMove(targetBootstrapper, localPath);
            BootstrapperHelper.FileSwap(exePath, localPath, silent);
        }
    }
}
