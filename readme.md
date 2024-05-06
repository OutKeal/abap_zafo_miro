# ZAFO_MIRO采购对账功能程序

## 业务需求概览

实现采购基于收货的对账功能.支持对账单的以下功能:

-   创建:查询未清GRIR明细数据,勾选行明细行创建对账单.
-   修改:对账单状态控制,支持作废,提交,取消提交等
-   查询:查询对账单明细
-   费用录入:支持录入正负费用,并按关键字分摊到采购明细行.
-   发票录入:支持发票号,金额,日期等关键信息录入,并校验与对账单金额一致.
-   过账:生成SAP发票校验,分摊差异金额到物料
-   支持打印输出

## 程序设计概要

-   主程序 ZAFO_MIRO:

    -   负责选择屏幕
    -   负责待对账明细的展示
    -   负责对账单屏幕展示和操作

-   类 ZAFO_MIRO_CLASS

    -   负责对账单所有逻辑实现

-   数据表

    -   ZAFO_MIRO_HEAD 对账单抬头表
    -   ZAFO_MIRO_ITEM 对账单明细表
    -   ZAFO_MIRO_INV 对账单发票明细
    -   ZAFO_MIRO_COST 对账单费用明细

-   配置表

    -   ZAFO_MIRO_COST_T 费用类型配置

-   CDS视图

    -   ZAFO_MIRO_TODO 待对账的采购清单明细
    -   ZAFO_MIRO_TODO_T 带文本的待对账的采购清单明细

## 主要功能实现

 ```mermaid
   graph LR
   选择屏幕--创建--->获取待对账明细--->展示选择待对账明细--->对账单屏幕
   选择屏幕--维护--->获取对账单明细--->对账单查询页面--->对账单屏幕
   对账单屏幕--->抬头信息--->状态管理
   对账单屏幕--->对账单采购明细--->支持数量拆分
   对账单屏幕--->对账单发票明细--->验证发票信息与对账金额一致
   对账单屏幕--->对账单费用明细--->分摊费用到采购明细
 ```

### 1. 选择屏幕


-   创建时:

    -   工厂	单选必填
    -   供应商	单选必填
    -   入库日期	多选
    -   采购订单	多选
    -   物料凭证	多选
    -   采购订单创建人	多选
    -   采购组	多选
    -   款号	多选

-   维护查询时:

    -   公司代码	多选
    -   供应商	多选
    -   对账单号	多选
    -   对账单状态	多选
    -   创建人	多选

### 2. 获取待对账单明细

通过选择屏幕条件和CDS视图ZAFO_MIRO_TODO_T获取对账单明细

-   关键表:

    -   EKKO 采购订单抬头
    -   EKPO 采购订单项目
    -   EKBE 采购订单历史
    -   EKKN 采购订单科目分配

-   关键选择条件:
    -   ekpo.menge <> 0,偶尔暂存凭证数量为0,作为除数会报错
    -   ekpo.repos = 'X',需要收货的采购订单
    -   ekpo.wepos = 'X'  需要开票的采购订单
    -   ekpo.loekz = ''  排除删除的采购订单
-   关键字段:
    -   ekbe.vgabe = '1' GR相关,代表采购相关物料凭证的数量(menge)/净额(dmbtr)
    -   ekbe.vgabe = '2' IR相关,代表采购相关物料凭证的数量(menge)/净额(arewr)

-   关键公式:
    -   通过关键字段分别求和GR的数量/净额,IR的数量/净额.
    -   通过EKPO的小计1(kzwi1)除以PO行数量(menge) 获取含税单价.
    -   GR数量/净额 减去 IR数量/净额,得出待对账的数量和净额.
    -   待对账的数量*单价,得出待对账的含税金额.


-   ZAFO_MIRO_TODO

    ```ABAP
    @AbapCatalog.sqlViewName: 'ZAFO_MIRO_TODO'
    @AbapCatalog.compiler.compareFilter: true
    @AbapCatalog.preserveKey: true
    @AccessControl.authorizationCheck: #NOT_REQUIRED
    @EndUserText.label: '待对账的采购清单'
    define view ZAFO_MIRO_CDS_TODO 
    as select from 
    ekko 
    inner join ekpo on ekko.ebeln = ekpo.ebeln 
    inner join ekbe on ekbe.ebeln = ekko.ebeln and ekbe.ebelp = ekpo.ebelp
    and ( ekbe.vgabe = '1' or ekbe.vgabe = '2' ) and ekbe.lfbnr is not initial
    left outer join ekkn on ekpo.ebeln = ekkn.ebeln and ekpo.ebelp = ekkn.ebelp and ekkn.zekkn = '01' 
     
    { 
    key ekko.mandt, 
    key ekbe.lfgja, 
    key ekbe.lfbnr,
    key ekbe.lfpos,
    ekbe.ebeln,
    ekbe.ebelp,
    ekko.ekorg,
    ekko.ekgrp,
    ekko.bukrs,
    ekko.ernam,
    ekko.aedat,
    ekko.lifnr,
    ekko.bsart,
    ekko.bedat,
    ekko.frgrl,
    ekpo.loekz,
    ekpo.pstyp,
    ekpo.knttp,
    ekpo.werks,
    @EndUserText.label: '需收货'
    ekpo.wepos,
    @EndUserText.label: '需发票'
    ekpo.repos,
    ekpo.matnr,
    @EndUserText.label: '物料名称'
    ekpo.txz01,
    ekpo.retpo,
    ekpo.bednr,
    ekpo.externalreferenceid as IHREZ,
    ekkn.aufnr,
    
    @EndUserText.label: '采购总数量'
    case ekpo.retpo
    when 'X' then - ekpo.menge
    else ekpo.menge 
    end as menge ,
    ekpo.meins ,
    @EndUserText.label: '含税单价'
    
    
    cast(
    case when ekpo.menge <> 0 
    then division( ekpo.kzwi1,ekpo.menge,2 ) 
    else 0 end 
    as farr_unit_price) as price,
    ekko.waers, 
    @EndUserText.label: '采购总金额'
    cast(
    case ekpo.retpo
    when 'X' then - ekpo.kzwi1 
    else ekpo.kzwi1  
    end 
    as bbwert ) as brtwr ,
    @EndUserText.label: '采购总金额'
    ekpo.netwr ,
    ekpo.mwskz,
    //@EndUserText.label: '税额'
    //
    //cast(
    //case ekpo.retpo
    //when 'X' then - ( ekpo.kzwi1 - ekpo.netwr )
    //else ekpo.kzwi1 - ekpo.netwr 
    //end 
    //as taxamount )as MWSKZ_AMOUNT ,
    
    
    @EndUserText.label: '入库数量'
    coalesce(
    cast
    (
    sum( case
    when ekbe.vgabe = '1' and ekbe.shkzg = 'H' then - ekbe.menge
    when ekbe.vgabe = '1' and ekbe.shkzg = 'S' then ekbe.menge
    end     ) as lqua_einme),0) as quantity_gr, 
    
    @EndUserText.label: '入库净额'
    coalesce(
    cast
    (
    sum( case
    when ekbe.vgabe = '1' and ekbe.shkzg = 'H' then - ekbe.dmbtr
    when ekbe.vgabe = '1' and ekbe.shkzg = 'S' then ekbe.dmbtr
    end ) as mm_a_gramount ),0) as net_gr,
    
    @EndUserText.label: '开票数量'
    
    coalesce(
    cast(sum( case
    when ekbe.vgabe = '2' and ekbe.shkzg = 'S' then ekbe.menge
    when ekbe.vgabe = '2' and ekbe.shkzg = 'H' then - ekbe.menge
    end  ) as remng),0) as quantity_ir,
    
    @EndUserText.label: '开票净额'
    coalesce(
    cast( sum( case
    when ekbe.vgabe = '2' and ekbe.shkzg = 'S' then ekbe.arewr
    when ekbe.vgabe = '2' and ekbe.shkzg = 'H' then -ekbe.arewr
    end )as mc_rewrt ),0) as net_ir
    }
    where ekpo.menge <> 0 --暂存凭证中可能为0
    and ekpo.repos = 'X'
    and ekpo.wepos = 'X' 
    group by
    ekko.mandt,
    ekbe.lfgja, 
    ekbe.lfbnr,
    ekbe.lfpos,
    ekbe.ebeln,
    ekbe.ebelp,
    ekko.ekorg,
    ekko.ernam,
    ekko.aedat,
    ekko.ekgrp,
    ekko.bukrs,
    ekko.lifnr,
    ekko.reswk,
    ekko.bsart,
    ekko.bedat,
    ekko.waers,
    ekko.frgrl,
    ekpo.ebelp,
    ekpo.loekz,
    ekpo.pstyp,
    ekpo.knttp,
    ekpo.werks,
    
    ekpo.wepos,
    ekpo.repos,
    ekpo.mwskz,
    
    ekpo.matnr,
    ekpo.txz01,
    ekpo.retpo,
    ekpo.bednr,
    ekpo.externalreferenceid,
    ekpo.menge,
    ekpo.meins,
    ekpo.kzwi1,
    ekpo.netwr,
    ekkn.aufnr
    ```

-   ZAFO_MIRO_TODO_T

    ```sql
    @AbapCatalog.sqlViewName: 'ZAFO_MIRO_TODO_T'
    @AbapCatalog.compiler.compareFilter: true
    @AbapCatalog.preserveKey: true
    @AccessControl.authorizationCheck: #NOT_REQUIRED
    @EndUserText.label: '待对账的采购清单,带文本'
    define view ZAFO_MIRO_CDS_TODO_T
    as select from 
    ZAFO_CDS_PO_MIRO as h
    left outer join lfa1 as a on h.lifnr = a.lifnr and h.lifnr <> ''
    left outer join t001 as b on h.bukrs = b.bukrs
    left outer join t001w as c on h.werks = c.werks
    left outer join t024e as e on h.ekorg = e.ekorg
    left outer join t024 as f on h.ekgrp = f.ekgrp
    left outer join t161t as g on h.bsart = g.bsart and g.bstyp = 'F' and g.spras = $session.system_language
    { 
    key h.mandt, 
    key h.lfgja, 
    key h.lfbnr,
    key h.lfpos,
    h.ebeln,
    h.ebelp,
    h.ekorg,
    @EndUserText.label: '采购组织名称'
    e.ekotx as ekorg_name, 
    h.ekgrp,
    @EndUserText.label: '采购织名称'
    f.eknam as ekgrp_name, 
    h.ernam, 
    h.aedat, 
    h.bukrs, 
    @EndUserText.label: '公司名称'
    b.butxt as bukrs_name,
    h.lifnr,
    @EndUserText.label: '供应商名称'
    a.name1 as lifnr_name,
    h.bsart,
    @EndUserText.label: '订单类型名称'
    g.batxt as bsart_name,
    h.bedat,
    h.waers,
    h.frgrl,
    h.loekz,
    h.pstyp,
    h.knttp,
    h.werks,
    @EndUserText.label: '工厂名称'
    c.name1 as werks_name,
    
    h.wepos,
    h.repos,
    h.matnr,
    h.txz01,
    h.retpo,
    h.bednr,
    h.IHREZ,
    @Semantics.quantity.unitOfMeasure:'meins'
    h.menge,
    h.meins,
    h.price,
    h.brtwr,
    h.netwr,
    h.mwskz,
    h.aufnr,
    
     @Semantics.quantity.unitOfMeasure:'meins'
    cast(h.quantity_gr as lqua_einme) as quantity_gr,
    cast(h.net_gr as mm_a_gramount) as net_gr,
    cast( division(h.quantity_gr * h.brtwr,h.menge,2) as amount_ca ) as amount_gr,
    
     @Semantics.quantity.unitOfMeasure:'meins'
    cast( h.quantity_ir as remng ) as quantity_ir ,
    cast( h.net_ir as mc_rewrt ) as net_ir,
    cast( division(h.quantity_ir * h.brtwr,h.menge,2) as amount_ca ) as amount_ir,
     @Semantics.quantity.unitOfMeasure:'meins'
    cast(h.quantity_gr - h.quantity_ir  as mmiv_open_invoice_quantity ) as quantity,
    cast(h.net_gr - h.net_ir as open_inv_amount_ca) as net,
    cast( division(h.quantity_gr * h.brtwr,h.menge,2)
    - division(h.quantity_ir * h.brtwr,h.menge,2) as amount_ca ) as amount
    }
    
    ```

### 3. 待对账明细数据展示

ALV展示对账明细数据,数据结构参照ZAFO_MIRO_TODO_T,ALV样式在配置表ZWFT_SCREEN中配置.

第一列复选框可编辑.

按钮:

-   全选
-   取消全选
-   确认选择

行信息从ZAFO_MIRO_HEAD/ZAFO_MIRO_ITEM中判断是否正在对账,如果是则显示为对账中,不可进入下一步.

点击确认选择后,将选择行提交到对账单屏幕.

### 4. 获取对账单明细数据,查询对账单信息

从对账单数据表ZAFO_MIRO_HEAD获取对账单数据.ALV展示.数据结构参照抬头表.ALV样式在配置表ZWFT_SCREEN中配置.

可以双击行或者单机单号,打开对账页面.

打开后获取另外三个表的信息(ITEM/COST/INV).

### 5.  对账单屏幕

包括抬头和三个明细ALV屏幕,可以用TAB页切换.

### 6. 对账单抬头

-   对账单号:只读,保存对账单后生成唯一流水,号段ZAFO_MIRO,双击可以打开GOS服务,关键本单生成的所有发票校验
-   币种:采购的唯一币种,只读
-   税码:采购的唯一税码,下拉可以修改.下拉值来自条件ZMST的税码配置.
-   记账日期:只读,默认当天,过账时修改回写.
-   对账日期:只读,默认当天.
-   发票校验号/年份:只读,过账后回写,冲销后清空.
-   公司代码/名称:只读,对账单的唯一公司代码
-   供应商/名称:只读,对账单的唯一供应商
-   对账单状态:只读,根据按钮更改(A初始,B已提交,C已审核,D已作废,L锁定)
-   总数量:只读,采购明细的总对账数量
-   费用金额:只读,对账采购明细中费用金额的金额汇总
-   总金额:只读,对账采购明细中总金额的汇总
-   对账金额:只读,对账采购明细中对账金额的汇总
-   对账税额:只读,对账采购明细中税额的汇总
-   发票金额:只读,对账发票明细中金额的汇总
-   发票税额:只读,对账发票明细中税额的汇总

### 7. 状态控制

-   状态清单:
    -   空
    -   A初始
    -   B已提交
    -   C已过账
    -   D已作废
    -   L已锁定
-   按钮清单
    -   保存(&SAVE)
    -   编辑(&EDIT)
    -   提交(&COMMIT)
    -   取消提交(&UNCOMMIT)
    -   过账(&POST)
    -   冲销(&CANCEL)
    -   作废(&DELETE)
-   打开单据时对ZAFO_MIRO_HEAD抬头加锁,锁定失败则状态为L,不可编辑

| 状态 | 描述   | 只读 | 可执行按钮                                                   | 执行后状态                                     |
| ---- | ------ | ---- | ------------------------------------------------------------ | ---------------------------------------------- |
| 空   | 空     |      | 保存(&SAVE)                                                  | A,只读                                         |
| A    | 初始   | X    | 编辑(&EDIT)<br />提交(&COMMIT)<br />作废(&DELETE)<br />打印(&PRINT) | A,非只读<br />B,只读<br />D,只读<br />状态无关 |
| A    | 初始   |      | 保存(&SAVE)                                                  | A,只读                                         |
| B    | 已提交 | X    | 取消提交(&UNCOMMIT)<br />过账(&POST)<br />打印(&PRINT)       | A,只读<br />C,只读<br />状态无关               |
| C    | 已过账 | X    | 冲销(&CANCEL)<br />打印(&PRINT)                              | A,只读<br />状态无关                           |
| D    | 作废   | X    | 无                                                           | 无                                             |

### 8. 对账采购明细

-   本页面参考表ZAFO_MIRO_ITEM,ALV样式在配置表ZWFT_SCREEN中配置.

-   只有对账数量可维护(单据在编辑状态),对账数量不超过待对账数量.
-   修改后重新计算对账的总额,净额,税额,重新分摊费用.

### 9. 对账发票明细

-   本页面参考表ZAFO_MIRO_INV,ALV样式在配置表ZWFT_SCREEN中配置.
-   字段逻辑:
    -   发票号
    -   发票金额,输入时若税额为空,则按照税率计算税额
    -   税额
    -   净额(发票金额-税额)
    -   发票日期
    -   备注(可选)

-   以上字段可选/必填可以在配置表中维护.
-   ALV按钮,新增行和删除行.
-   修改发票明细后,自动重算抬头金额.

### 10. 对账费用明细

-   本页面参考表ZAFO_MIRO_COST,ALV样式在配置表ZWFT_SCREEN中配置.
-   字段逻辑:
    -   费用类型:下拉,数据来自于配置表ZAFO_MIRO_COST_T.
    -   费用金额
    -   费用分摊方式:对账单/采购订单/物料编号/款号
    -   分摊关键字:根据分摊方式不同控制方式不同
        -   对账单:必空
        -   采购订单:非空,F4筛选ITEM中的唯一采购订单号
        -   物料编号:非空,F4筛选ITEM中的唯一物料编号
        -   款号:非空,F4筛选ITEM中的唯一款号(采购跟踪号)
    -   费用备注
-   以上字段可选/必填可以在配置表中维护.
-   ALV按钮,新增行和删除行.
-   修改费用明细后,自动重算ITEM中的费用金额值,重算抬头金额.

### 11. 计算公式

ZAFO_MIRO_CLASS->CALCULATE( ).方法

采购对账明细,发票明细,费用明细,抬头税率等修改后,都触发计算逻辑.

计算顺序如下:

1.   根据税码获取税率

2.   分摊金额

     -   循环费用明细,将每一行费用按照分摊关键字确定自己的分摊ITEM行,按对账金额比例分摊到ITEM的费用金额(COST)字段.

3.   循环计算采购明细行

     -   若对账数量 = GR数量 - IR数量(代表完全对账)
         -   对账金额(含费用) = GR金额 - IR金额
         -   对账净额(含费用) = GR净额 - IR净额
     -   若对账数量 <> GR数量 - IR数量(代表不完全对账)
         -   对账金额(含费用) = 对账数量 * 采购总金额 / 采购总数量
         -   对账金额(含费用) = 对账数量 * 采购总净额 / 采购总数量
     -   对账金额 = 总金额 + 费用金额
     -   对账税额 = 对账金额 * 税率 / ( 100 + 税率 )
     -   对账净额 = 对账金额 - 对账税额

4.   抬头字段计算公式

     -   数量 = SUM( 采购明细-对账数量)
     -   总金额 = SUM( 采购明细-对账金额(含费用))
     -   费用金额 = SUM( 采购明细-费用金额)
     -   对账金额 = SUM( 采购明细-对账金额)
     -   对账净额 = SUM( 采购明细-对账净额)
     -   对账税额 = SUM( 采购明细-对账税额)
     -   发票金额 = SUM( 发票明细-发票金额)
     -   发票税额 = SUM( 发票明细-发票税额)

### 12. 按钮功能

-   保存(&SAVE)

    -   触发计算逻辑
    -   通过配置,检查明细字段的必填性,报错类型为警告.
    -   补行号,保存数据到表(HEAD/ITEM/INV/COST),加锁,设置只读
    -   弹出消息

-   编辑(&EDIT)

    -   设置只读为空

-   删除(&DELETE)

    -   设置只读
    -   设置状态为D

-   提交(&COMMIT)

    -   通过配置,检查明细字段的必填性,报错类型为错误,报错则停止.
    -   设置状态为B

-   取消提交(&UNCOMMIT)

    -   设置状态为A

-   过账(&POST)

    -   调用BAPI生成发票校验
    -   对应ITEM中的COST费用金额,按以下逻辑分摊:
        -   物料编号不为空,工厂/物料号/单位/数量/费用金额写入发票校验物料调整页签
        -   对于物料为空但工单不为空的行,获取该工单AFPO的入库明细(没入库则按计划明细),按照数量比例分摊到工单的SKU中,工厂/物料号/单位/数量/费用金额写入发票校验物料调整页签
        -   对于物料/工单都为空的行,直接调整发票校验的标准采购明细.

    -   返回发票校验号,到HEAD表和屏幕抬头
    -   绑定GOS服务,本单到发票校验单据.

-   冲销(&CANCEL)

    -   调用BAPI冲销发票校验
    -   清空抬头表HEAD和抬头页面的发票校验号/年份.
    -   绑定GOS服务,冲销凭证到本对象单据号.

-   打印(&PRINT)

    -   调用ZWFT_HTML类将抬头/明细等字段网页数据预览.

    



 





