(ns jibbit.layer
  (:import (java.nio.file Path Files LinkOption)
           (com.google.cloud.tools.jib.api.buildplan
             AbsoluteUnixPath
             FileEntriesLayer$Builder
             FilePermissions)
           (com.google.cloud.tools.jib.api LayerConfiguration$Builder)
           (java.nio.file.attribute FileAttribute)))

(def no-file-attrs (into-array FileAttribute []))

(defonce empty-dir (Files/createTempDirectory "empty" no-file-attrs))

(defn add-permissioned-entry [ b empty-dir npath perm]
  (if (some? perm)
    (.addEntry b empty-dir npath perm)
    (.addEntry b empty-dir npath )))

(declare add-map-layer!)
(defn handle-item [^FileEntriesLayer$Builder b ^AbsoluteUnixPath npath perm v]
  (cond
    (map? v)
    (do
       (add-permissioned-entry b empty-dir npath perm)
      (add-map-layer b npath v))
    (instance? Path v)
    (if (Files/isDirectory v (into-array LinkOption []))
      (.addEntryRecursive b v npath)
      (add-permissioned-entry b v npath perm))
    (nil? v)
    (add-permissioned-entry b empty-dir npath perm)
    (vector? v)
    (run!
      #(handle-item ^FileEntriesLayer$Builder b ^AbsoluteUnixPath npath perm %)
      v)))

(defn add-map-layer! [^FileEntriesLayer$Builder b ^AbsoluteUnixPath path m]
  (reduce-kv
    (fn [_ [k v]]
      (let [filename (if (string? k)
                       k
                       (do
                         (assert (vector? k))
                         (nth k 0)))
            permission (when (vector? k)
                         (nth k 1))]
        (handle-item ^LayerConfiguration$Builder b
                     ^AbsoluteUnixPath (.resolve path filename)
                     (when permission (FilePermissions/fromOctalString permission))
                     v)))
    b
    m)
  nil)

(defn add-map-layer [^FileEntriesLayer$Builder b {:keys [^AbsoluteUnixPath root-path layer-map]}]
  (add-map-layer! b (AbsoluteUnixPath/get root-path ) layer-map))
