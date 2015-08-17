import Neovim

import qualified Fibonacci as Fibonacci
import qualified Random    as Random

main :: IO ()
main = neovim defaultConfig
    { plugins = plugins defaultConfig ++ [ Fibonacci.plugin, Random.plugin ]
    }
