repos_list = c("sdc.food_dev")
repos_dir = "./repos"  # directory that contains repos

# if . in repos_dir
if(grepl(".", repos_dir))  
{
  cwd = getwd()
  repos_dir = sub(".", cwd, repos_dir)  # input path
}

for(repo in repos_list)
{
  setwd(repos_dir)
  
  # if repo was already cloned, pull.  Otherwise, clone
  
  if(file.exists(repo))
  {
    print(paste("pulling", repo, "..."))
    d = paste0(repos_dir, "/", repo)
    setwd(d)
    system("git pull")
    setwd(repos_dir)
  }
  else
  {
    print(paste("cloning", repo, "..."))
    clone_str = paste0("git clone https://", Sys.getenv('PAT'), "@github.com/uva-bi-sdad/", repo, ".git")
    system(clone_str)
  }
  
  setwd(cwd)
}

