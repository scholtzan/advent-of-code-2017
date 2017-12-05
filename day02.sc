val in = "104\t240\t147\t246\t123\t175\t372\t71\t116\t230\t260\t118\t202\t270\t277\t292\n740\t755\t135\t205\t429\t822\t844\t90\t828\t115\t440\t805\t526\t91\t519\t373\n1630\t991\t1471\t1294\t52\t1566\t50\t1508\t1367\t1489\t55\t547\t342\t512\t323\t51\n1356\t178\t1705\t119\t1609\t1409\t245\t292\t1434\t694\t405\t1692\t247\t193\t1482\t1407\n2235\t3321\t3647\t212\t1402\t3711\t3641\t1287\t2725\t692\t1235\t3100\t123\t144\t104\t101\n1306\t1224\t1238\t186\t751\t734\t1204\t1275\t366\t149\t1114\t166\t1118\t239\t153\t943\n132\t1547\t1564\t512\t2643\t2376\t2324\t2159\t1658\t107\t1604\t145\t2407\t131\t2073\t1878\n1845\t91\t1662\t108\t92\t1706\t1815\t1797\t1728\t1150\t1576\t83\t97\t547\t1267\t261\n78\t558\t419\t435\t565\t107\t638\t173\t93\t580\t338\t52\t633\t256\t377\t73\n1143\t3516\t4205\t3523\t148\t401\t3996\t3588\t300\t1117\t2915\t1649\t135\t134\t182\t267\n156\t2760\t1816\t2442\t2985\t990\t2598\t1273\t167\t821\t138\t141\t2761\t2399\t1330\t1276\n3746\t3979\t2989\t161\t4554\t156\t3359\t173\t3319\t192\t3707\t264\t762\t2672\t4423\t2924\n3098\t4309\t4971\t5439\t131\t171\t5544\t595\t154\t571\t4399\t4294\t160\t6201\t4329\t5244\n728\t249\t1728\t305\t2407\t239\t691\t2241\t2545\t1543\t55\t2303\t1020\t753\t193\t1638\n260\t352\t190\t877\t118\t77\t1065\t1105\t1085\t1032\t71\t87\t851\t56\t1161\t667\n1763\t464\t182\t1932\t1209\t640\t545\t931\t1979\t197\t1774\t174\t2074\t1800\t939\t161"


def checksum1(input: String): Int = {
  val rows = input.split("\n")
  val res = rows.map { row =>
    val values = row.split("\t").map(_.toInt)
    values.max - values.min
  }

  res.sum
}


def test1() = {
  val testInput = "5\t1\t9\t5\n7\t5\t3\n2\t4\t6\t8"

  assert(checksum1(testInput) == 18)
}


def checksum2(input: String): Int = {
  val rows = input.split("\n")
  val res = rows.flatMap { row =>
    val values = row.split("\t").map(_.toInt).combinations(2)

    values.map { case Array(x, y) =>
      if (x % y == 0) {
        x / y
      } else if (y % x == 0) {
        y / x
      }
      else {
        0
      }
    }
  }

  res.sum
}



def test2() = {
  val testInput = "5\t9\t2\t8\n9\t4\t7\t3\n3\t8\t6\t5"

  assert(checksum2(testInput) == 9)
}


test1()

println("Result: " + checksum1(in))

test2()

println("Result: " + checksum2(in))