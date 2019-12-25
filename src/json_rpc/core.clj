(ns json-rpc.core
  (:require [promesa.core :as p]
            [clojure.string :as str])
  (:import
   [java.nio.channels
    AsynchronousServerSocketChannel
    AsynchronousSocketChannel
    SocketChannel
    CompletionHandler
    AsynchronousCloseException]
   [java.net InetSocketAddress]
   [java.nio ByteBuffer]))

(set! *warn-on-reflection* true)

(defn client [^String host ^Integer port]
  (let [cl-addr (InetSocketAddress. host port)
        cl (SocketChannel/open cl-addr)]
    cl))

(defn client-send [^SocketChannel cl data]
  (let [^ByteBuffer buf (ByteBuffer/wrap data)]
    (try (.write cl buf)
         (catch Exception e
           (.close cl)))))

(defn parse-headers [lines]
  {:content-length 20})

;; (def line-sep "\r\n")
;; (def header-sep "\r\n\r\n")
(def line-sep "\n")
(def header-sep "\n\n")


;; (defn parse-channel [{prev-msg :msg prev-headers :headers prev-state :state} new-msg]
;;   (loop [result []
;;          msg (str prev-msg new-msg)
;;          headers prev-headers
;;          state prev-state]
;;     (cond
;;       (= state :wait-for-headers)
;;       (if-let [headers-end-idx (str/index-of msg header-sep)] ;; "\r\n\r\n"
;;         (let [raw-headers (subs msg 0 headers-end-idx)
;;               rest-msg (subs msg (+ headers-end-idx (count header-sep)))
;;               headers (parse-headers raw-headers)]
;;           (recur result rest-msg headers :wait-for-body))
;;         [result
;;          {:state state
;;           :msg msg
;;           :headers nil}])
;;       (= state :wait-for-body)
;;       (let [body-end-idx (:content-length headers)]
;;         (if (< body-end-idx (count msg))
;;           (let [raw-body (subs msg 0 body-end-idx)
;;                 rest-msg (subs msg body-end-idx)
;;                 body raw-body]
;;             (recur (conj result {:headers headers :body body}) rest-msg nil :wait-for-headers))
;;           [result
;;            {:state state
;;             :msg msg
;;             :headers headers}])))))


(defn parse-channel [prev-ctx new-msg]
  (loop [result []
         ctx (update prev-ctx :msg #(str % new-msg))]
    (let [{:keys [state msg headers]} ctx]
      (condp = state
        :wait-for-headers
        (if-let [headers-end-idx (str/index-of msg header-sep)] ;; "\r\n\r\n"
          (let [raw-headers (subs msg 0 headers-end-idx)
                rest-msg (subs msg (+ headers-end-idx (count header-sep)))
                headers (parse-headers raw-headers)]
            (recur result (assoc ctx :msg rest-msg :headers headers :state :wait-for-body)))
          [result ctx])
        :wait-for-body
        (let [body-end-idx (:content-length headers)]
          (if (< body-end-idx (count msg))
            (let [raw-body (subs msg 0 body-end-idx)
                  rest-msg (subs msg body-end-idx)
                  body raw-body]
              (recur (conj result {:headers headers :body body})
                     (assoc ctx :msg rest-msg :headers nil :state :wait-for-headers)))
            [result ctx]))))))


(defn read-channel [^AsynchronousSocketChannel channel size enqueue conns]
  (let [buf (ByteBuffer/allocateDirect size)
        ctx (atom {:state :wait-for-headers ;; :wait-for-body
                   :msg ""
                   :headers nil})]
    (println "read channel")
    (.read channel buf nil
           (reify CompletionHandler
             (completed [this cnt _]
               (println "Completed" cnt)
               (when (= -1 cnt)
                 (println "Disconnected " channel)
                 (swap! conns disj channel))
               (when (> cnt 0)
                 (let [bytes (byte-array cnt)]
                   (println "\n-------------before:" "\n--------------------------\n")
                   (clojure.pprint/pprint @ctx)
                   (.flip buf)
                   (.get buf bytes)
                   (let [[results new-state] (parse-channel @ctx (String. bytes))]
                     (println "\n------------------After:" "\n--------------------------\n")
                     (clojure.pprint/pprint new-state)
                     (reset! ctx new-state))
                   #_(enqueue bytes)
                   (.clear buf))
                 (.read channel buf nil this)))
             (failed [this e _]
               (if (instance? AsynchronousCloseException e)
                 (println "Closed " channel)
                 (do (.close channel)
                     (println "! Failed (read):" e))))))))

(defn handler [listener enqueue conns]
  (reify CompletionHandler
    (failed [this e _]
      (if (instance? AsynchronousCloseException e)
        (println "Closed..")
        (println "! Failed (read):" e)))
    (completed [this sc _] ;; sc - clientChannel
      (println "Incomming connection " sc)
      (swap! conns conj sc)
      (.accept ^AsynchronousServerSocketChannel listener nil  this)
      (read-channel sc 30 enqueue conns))))

(defn start [ctx]
  (let [assc (AsynchronousServerSocketChannel/open)
        port 7345
        sa  (InetSocketAddress. port)
        listener (.bind assc sa)
        enqueue (or (:enqueue @ctx) println)
        conns (atom #{})]
    (println "tcp logs server started at  " port)
    (.accept listener nil (handler listener enqueue conns))
    (swap! ctx (fn [ctx] (update ctx :zmq assoc :sock assc :conns conns))))
  ctx)


(defn stop [ctx]
  (when-let [conns (get-in @ctx [:zmq :conns])]
    (doseq [c @conns]
      (.close ^AsynchronousSocketChannel c))
    (reset! conns #{}))

  (when-let [sock (get-in @ctx [:zmq :sock])]
    (println "Stop server")
    (.close ^AsynchronousSocketChannel sock)
    (println "ok")))

(comment
  (stop ctx)

  (def ctx (start (atom {:zmq {:port 7345}
                         :enqueue (fn [msg] (println "Incomming " (String. msg)))})))

  ctx

  (def cl (client "localhost" 7777))

  (.close cl)

  (.isOpen cl)
  (.isConnected cl)

  (client-send cl (for [i (range 100)] {:a i}))


  )


