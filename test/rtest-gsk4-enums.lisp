(in-package :gtk-test)

(def-suite gsk-enums :in gsk-suite)
(in-suite gsk-enums)

;;; --- Types and Values -------------------------------------------------------

;;;     GskIsolation                                        Since 4.22

(test gsk-isolation
  ;; Check type
  (is (g:type-is-flags "GskIsolation"))
  ;; Check registered name
  (is (eq 'gsk:isolation
          (glib:symbol-for-gtype "GskIsolation")))
  ;; Check type initializer
  (is (eq (g:gtype "GskIsolation")
          (g:gtype (cffi:foreign-funcall "gsk_isolation_get_type" :size))))
  ;; Check names
  (is (equal '("GSK_ISOLATION_NONE"
               "GSK_ISOLATION_BACKGROUND"
               "GSK_ISOLATION_COPY_PASTE"
               "GSK_ISOLATION_ALL")
             (glib-test:list-flags-item-names "GskIsolation")))
  ;; Check values
  (is (equal '(0 1 2 4294967295)
             (glib-test:list-flags-item-values "GskIsolation")))
  ;; Check nick names
  (is (equal '("none" "background" "copy-paste" "all")
             (glib-test:list-flags-item-nicks "GskIsolation")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "GskIsolation" GSK:ISOLATION
                                     (:EXPORT T
                                      :TYPE-INITIALIZER "gsk_isolation_get_type")
                                     (:NONE 0)
                                     (:BACKGROUND 1)
                                     (:COPY-PASTE 2)
                                     (:ALL 4294967295))
             (gobject:get-gtype-definition "GskIsolation"))))

;;;     GskPorterDuff                                       Since 4.22

(test gsk-porter-duff
  ;; Check type
  (is (g:type-is-enum "GskPorterDuff"))
  ;; Check type initializer
  (is (eq (g:gtype "GskPorterDuff")
          (g:gtype (cffi:foreign-funcall "gsk_porter_duff_get_type" :size))))
  ;; Check registered name
  (is (eq 'gsk:porter-duff
          (glib:symbol-for-gtype "GskPorterDuff")))
  ;; Check names
  (is (equal '("GSK_PORTER_DUFF_SOURCE"
               "GSK_PORTER_DUFF_DEST"
               "GSK_PORTER_DUFF_SOURCE_OVER_DEST"
               "GSK_PORTER_DUFF_DEST_OVER_SOURCE"
               "GSK_PORTER_DUFF_SOURCE_IN_DEST"
               "GSK_PORTER_DUFF_DEST_IN_SOURCE"
               "GSK_PORTER_DUFF_SOURCE_OUT_DEST"
               "GSK_PORTER_DUFF_DEST_OUT_SOURCE"
               "GSK_PORTER_DUFF_SOURCE_ATOP_DEST"
               "GSK_PORTER_DUFF_DEST_ATOP_SOURCE"
               "GSK_PORTER_DUFF_XOR"
               "GSK_PORTER_DUFF_CLEAR")
             (glib-test:list-enum-item-names "GskPorterDuff")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11)
             (glib-test:list-enum-item-values "GskPorterDuff")))
  ;; Check nick names
  (is (equal '("source" "dest" "source-over-dest" "dest-over-source"
               "source-in-dest" "dest-in-source" "source-out-dest"
               "dest-out-source" "source-atop-dest" "dest-atop-source"
               "xor" "clear")
             (glib-test:list-enum-item-nicks "GskPorterDuff")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "GskPorterDuff" GSK:PORTER-DUFF
                                    (:EXPORT T
                                     :TYPE-INITIALIZER "gsk_porter_duff_get_type")
                                    (:SOURCE 0)
                                    (:DEST 1)
                                    (:SOURCE-OVER-DEST 2)
                                    (:DEST-OVER-SOURCE 3)
                                    (:SOURCE-IN-DEST 4)
                                    (:DEST-IN-SOURCE 5)
                                    (:SOURCE-OUT-DEST 6)
                                    (:DEST-OUT-SOURCE 7)
                                    (:SOURCE-ATOP-DEST 8)
                                    (:DEST-ATOP-SOURCE 9)
                                    (:XOR 10)
                                    (:CLEAR 11))
             (gobject:get-gtype-definition "GskPorterDuff"))))

;;; 2026-08-30
