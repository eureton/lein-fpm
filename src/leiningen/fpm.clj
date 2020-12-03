(ns leiningen.fpm
  (:require
    [clojure.string :as string]
    [clojure.java.io :as io]
    [clojure.java.shell :as shell]
    [me.raynes.fs :as fs]
    [leiningen.core.project :as project]
    [clojure.tools.cli :refer [parse-opts]]
    [leiningen.uberjar :as uberjar]))

(defn- jar-file-name
  "The filename of the standalone jar for the project."
  [project]
  (let [project-name (:name project)
        version (:version project)
        basename (string/join "-" [project-name version "standalone"])]
    (str basename ".jar")))

(defn- jar-path
  "The full path to the jar on this system."
  [project]
  (io/file (:target-path project) (jar-file-name project)))

(defn- jar-destination-path
  "The full path to the jar once installed."
  [project]
  (io/file "/usr/lib" (:name project) (jar-file-name project)))

(defn- bin-path
  "The full path to the wrapper binary on this system."
  [project]
  (io/file (:target-path project) (:name project)))

(defn- bin-destination-path
  "The full path to the binary wrapper once installed."
  [project]
  (io/file "/usr/bin" (:name project)))

(defn- upstart-path
  "The full path to the upstart script on this system."
  [project]
  (io/file (:target-path project) (str (:name project) ".conf")))

(defn- upstart-destination-path
  "The full path to the upstart script once installed."
  [project]
  (io/file "/etc/init" (str (:name project) ".conf")))

(defn- package-path
  "The full path to the output package."
  [project package-type]
  (io/file (:target-path project)
           (str (:name project) "_" (:version project) "." package-type)))

(defn- jar-invocation
  "The command to run the JAR once installed."
  [project]
  (let [jar-path (jar-destination-path project)
        opts (:jvm-opts project)
        init-string (if (not (empty? opts))
                      (str "java " (string/join " " opts) " -jar")  
                      "java -jar")]
    (string/join " " [init-string jar-path "$@"])))

(defn wrapper-binary
  "Writes a wrapper binary to :target-path/bin and returns its path."
  [project]
  (let [file (bin-path project)
        shebang "#!/bin/bash"
        contents (str shebang "\n" (jar-invocation project) "\n")
        _ (fs/mkdirs (fs/parent file))
        _ (spit file contents)
        _ (fs/chmod "+x" file)]
    file))

(defn upstart-script
  "Writes an upstart script and returns its path."
  [project]
  (let [file (upstart-path project)
        project-name (:name project)
        contents (str "#!upstart\n\n"
                      "description \"" project-name "\"\n"
                      "start on startup\n"
                      "stop on shutdown\n"
                      "respawn\n"
                      "exec /usr/bin/" project-name "\n")
        _ (fs/mkdirs (fs/parent file))
        _ (spit file contents)]
    file))

(defn- default-dependencies
  "Default package dependencies for the provided package type."
  [package-type]
  (condp = package-type
    "deb" "openjdk-7-jre-headless"
    "rpm" "java-1.7.0-openjdk"
    "solaris" "jdk-7"))

(defn- fpm-options
  "The options to be passed to fpm. Returns a vector of vectors instead of a
  map to support multiple instances of the same option."
  [project options]
  (let [maintainer (:maintainer options)]
    (->>
      [["-s" "dir"]
       ["-t" (:type options)]
       (when maintainer ["-m" maintainer])
       ["--force"]
       ["-a" "all"]
       ["-p" (str (package-path project (:type options)))]
       ["-n" (:name project)]
       ["-v" (:version project)]
       ["--url" (:url project)]
       ["--description" (:description project)]
       ; TODO replace with :multi true cli parser configuration, when available
       (map
         #(vector "-d" %)
         (string/split
           (:package-dependencies options (default-dependencies (:type options)))
           #","))
       ["--rpm-os" "linux"]]
      (remove nil?))))

(defn- parameters
  "The parameters to be passed to fpm."
  [project]
  (let [jar-src (jar-path project)
        jar-dest (jar-destination-path project)
        bin-src (bin-path project)
        bin-dest (bin-destination-path project)
        upstart-src (upstart-path project)
        upstart-dest (upstart-destination-path project)]
    [(str jar-src "=" jar-dest)
     (str bin-src "=" bin-dest)
     (str upstart-src "=" upstart-dest)]))

(defn- warnln
  "Prints a message to stderr."
  [message]
  (binding [*out* *err*]
    (println message)))

(defn fpm-command-strings
  "Returns the fpm command as a sequence of tokens."
  [project options]
  (concat ["fpm"]
          (flatten (fpm-options project options))
          (parameters project)))

(defn package
  "Invokes fpm to build the package and returns the resulting path."
  [project options]
  (let [command-strings (fpm-command-strings project options)
        {:keys [exit out err]} (apply shell/sh command-strings)]
    (when-not (empty? out)
      (println (string/trim-newline out)))
    (when-not (empty? err)
      (warnln (string/trim-newline err)))
    (when (pos? exit)
      (warnln "Failed to build package!")
      (System/exit exit))
    (package-path project (:type options))))

(def cli-options
  [["-d" "--pkg-dependency PKG"
    "comma-separated list of packages it depends on"
    :id :package-dependencies
    :default-desc "depends on type, see below"]
   ["-m" "--maintainer EMAIL"
    "email of maintainer to mark package with"
    :validate-fn #(re-matches #"^[^@\s]+@[^@\s]+\.[^@\s]+$" %)
    :validate-msg "must be a valid email address"]
   [nil "--dry-run"
    "print the fpm command to stdout instead of running it"
    :default false]])

(defn usage
  "Combines the usage text of the plugin and its options into a single string."
  [options-summary]
  (->> ["Usage: lein fpm [options] type"
        ""
        "Options:"
        options-summary
        ""
        "Types:"
        "  deb       Create a Debian package (default)"
        "  rpm       Create an RPM package"
        "  solaris   Create a Solaris package"
        ""
        "Default package dependencies:"
        "  deb       default-jre"
        "  rpm       java-1.7.0-openjdk"
        "  solaris   jdk-7"]
       (string/join \newline)))

(defn error-msg
  "Turns clojure.tools.cli errors into a single error message."
  [errors]
  (str
    "The following errors occurred while parsing your command:\n\n"
    (string/join \newline errors)))

(defn validate-args
  "Validates command-line arguments and returns a map:
    * either indicating that the task should exit
      {:exit-message msg :ok? [true|false]}
    * or containing the provided information
      {:options {:type TYPE :package-dependencies DEPS ...}}"
  [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)
        package-type (first arguments)
        usage-summary (usage summary)]
    (cond
      (= package-type "help")
      {:exit-message usage-summary :ok? true}

      errors
      {:exit-message (error-msg errors) :ok? false}

      (and
        (= 1 (count arguments))
        (#{"deb" "rpm" "solaris"} package-type))
      {:options (assoc options :type package-type)}

      :else
      {:exit-message usage-summary})))

(defn exit-with-message
  "Prints to stderr and exits with the given code."
  [code message]
  ((if (= code 0) println warnln) message)
  (System/exit code))

(defn build
  "Generates the package for the given project and options."
  [project options]
  (let [project (project/set-profiles project [:uberjar])]
    (println "Creating uberjar")
    (uberjar/uberjar project)
    (println "Creating wrapper binary")
    (wrapper-binary project)
    (println "Creating upstart script")
    (upstart-script project)
    (println "Building package")
    (package project options)))

(defn fpm
  "Generates a minimalist package for the current project."
  ([project] (fpm project "deb"))
  ([project & args]
   (let [{:keys [options exit-message ok?]} (validate-args args)]
     (when exit-message
       (exit-with-message (if ok? 0 1) exit-message))
     (when (:dry-run options)
       (exit-with-message
         0
         (string/join \space (fpm-command-strings project options))))
     (build project options))))
