;;; guix-forge --- Guix software forge meta-service
;;; Copyright © 2022 Arun Isaac <arunisaac@systemreboot.net>
;;;
;;; This file is part of guix-forge.
;;;
;;; guix-forge is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; guix-forge is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with guix-forge.  If not, see
;;; <https://www.gnu.org/licenses/>.

(define-module (forge socket)
  #:use-module (guix records)
  #:export (<forge-ip-socket>
            forge-ip-socket
            forge-ip-socket?
            forge-ip-socket-ip
            forge-ip-socket-port
            ipv4-address?
            ipv6-address?
            <forge-unix-socket>
            forge-unix-socket
            forge-unix-socket?
            forge-unix-socket-path))

(define-record-type* <forge-ip-socket>
  forge-ip-socket make-forge-ip-socket
  forge-ip-socket?
  (ip forge-ip-socket-ip
      (default "127.0.0.1"))
  (port forge-ip-socket-port))

(define (ipv4-address? address)
  "Return #t if ADDRESS is an IPv4 address. Else, return #f."
  (string-contains? address "."))

(define (ipv6-address? address)
  "Return #t if ADDRESS is an IPv6 address. Else, return #f."
  (string-contains? address ":"))

(define-record-type* <forge-unix-socket>
  forge-unix-socket make-forge-unix-socket
  forge-unix-socket?
  (path forge-unix-socket-path))
