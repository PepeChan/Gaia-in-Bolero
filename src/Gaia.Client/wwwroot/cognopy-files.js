export function saveJsonFile(filename, content) {
  const blob = new Blob([content], { type: "application/json;charset=utf-8" });
  const url = URL.createObjectURL(blob);
  const anchor = document.createElement("a");

  anchor.href = url;
  anchor.download = filename;
  anchor.style.display = "none";

  document.body.appendChild(anchor);
  anchor.click();
  anchor.remove();

  window.setTimeout(() => URL.revokeObjectURL(url), 0);
}

export function openJsonFile() {
  return new Promise((resolve, reject) => {
    const input = document.createElement("input");

    input.type = "file";
    input.accept = ".json,application/json";
    input.style.display = "none";

    const cleanup = () => {
      input.remove();
    };

    input.addEventListener(
      "change",
      () => {
        const file = input.files && input.files.length > 0 ? input.files[0] : null;
        cleanup();

        if (!file) {
          resolve(null);
          return;
        }

        const reader = new FileReader();

        reader.onload = () => {
          resolve(typeof reader.result === "string" ? reader.result : "");
        };

        reader.onerror = () => {
          reject(new Error("Could not read selected project file."));
        };

        reader.readAsText(file);
      },
      { once: true }
    );

    input.addEventListener(
      "cancel",
      () => {
        cleanup();
        resolve(null);
      },
      { once: true }
    );

    document.body.appendChild(input);
    input.click();
  });
}

window.cognopyFiles = {
  saveJsonFile,
  openJsonFile,
};
