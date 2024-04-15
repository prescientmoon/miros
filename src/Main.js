import { exec } from "child_process";

export const runCommandImpl = (command) => {
  return (onError, onSuccess) => {
    exec(command, (error, stdout, stderr) => {
      if (error) {
        onError(error.message);
      } else if (stderr) {
        onError(stderr);
      } else {
        onSuccess(stdout);
      }
    });
  };
};
