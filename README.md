# C-net
__C-net programming language__ - Network programming never got easier
![GitHub Workflow Status (branch)](https://img.shields.io/github/workflow/status/bruk3/C-net/cnet-ci/main)

## Environment setup 


### Instructions to merge code with master
Since 5 people have to contribute code to the same repository, things can get messy really easily. That's why we should try to enforce certain coding guidelines. 

We can follow a simple workflow where each person creates a feature branch and makes pull requests to master. This simple [git collaboration workflow](https://gist.github.com/adamloving/5690951) basically has all the git commands you'd need (The only thing missing is `git pull`). 

Things to note:
- Create temporary feature branches for implementing small but __working__ features. Once you've implemented this task and your pull request has been merged to master, you should delete your current feature branch and create a new one for your next feature.
- When you make a pull request, an automated github CI action will immediately start running. You can learn more about github CI actions [here](https://lab.github.com/githubtraining/github-actions:-continuous-integration). The github action will automatically run the tests provided in your repository and makes sure that all of the tests pass before the pull request can get merged.

#### Commit Messages 
Having standarized commit messages that are easy to read and understand can be super helpful and easy to implement.
By taking 5 more seconds to write better commit messages, we can make each other's lives easier. 

Check out [The Seven Rules of a Great Commit Message](https://chris.beams.io/posts/git-commit/#separate).
Tbh, just following 2, 3, and 5 should be enough!



### Suggested Development Environment 

#### Install [VSCode](https://code.visualstudio.com/) for code editor. 
__Why VSCode?__ 
- Automated Linting (Has support for ocaml plugins)
- Code Completion
- In-File Git commit version comparison
- User friendly interface to resolve merge conflicts
- If you're a VIM junkie, has support for VIM plugins
- When we want to help each other debug code by sharing screen, having a similar dev environment would allow the other person to jump right in. 

#### Instructions for VSCode 

VSCode Extensions to install. Use the (Ctrl + Shift + P)/(Command + Shift + P) shortcut to open the command palette and then search for `Install Extensions`. In the install extensions tab, you can go ahead and install the following VSCode Extensions. 

- Ocaml and Reason IDE (Syntax highlighting, autoformatting & Code completion)
- Ocaml Platform (Syntax highlighting for ocamllex files - .mll, .mli, .mly)
- Vim



### Team members 
1. Rediet Bekele
2. William Oseghare
3. Kidus Mulu
4. Kingsley Neequaye
5. Bruk Zewdie 

