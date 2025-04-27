module AXI (
  //************
  //与总线的交互
  //************
  input  wire ACLK,
  input  wire ARESETn,

  //AW    from master to slave
  output wire [3:0]   AWID,
  output wire [31:0]  AWADDR,
  output wire [7:0]   AWLEN,
  output wire [2:0]   AWSIZE,
  output wire [1:0]   AWBURST,
  output wire         AWLOCK,
  output wire [3:0]   AWCACHE,
  output wire [2:0]   AWPORT,

  output wire         AWVALID,
  input  wire         AWREADY,

  //W     from master to slave
  output wire [31:0]  WDATA,
  output wire [3:0]   WSTRB,
  output wire         WLAST,
  input  wire         WREADY,
  output wire         WVALID,

  //B     from slave to master
  input  wire [3:0]   BID,
  input  wire [1:0]   BRESP,
  input  wire         BVALID,
  output wire         BREADY,

  //AR    from master to slave
  output wire [3:0]   ARID,
  output wire [31:0]  ARADDR,
  output wire [7:0]   ARLEN,
  output wire [2:0]   ARSIZE,
  output wire [1:0]   ARBURST,
  output wire         ARLOCK,
  output wire [3:0]   ARCACHE,
  output wire [2:0]   ARPROT,
  output wire         ARVALID,
  input  wire         ARREADY,

  //R     from slave to master
  input  wire [3:0]   RID,
  input  wire [31:0]  RDATA,
  input  wire [1:0]   RRESP,
  input  wire         RLAST,
  input  wire         RVALID,
  output wire         RREADY,

  //*************
  //与控制的交流
  //*************
  input wire rd_req,
  input wire [8:0] rd_len,
  input wire [19:0] rd_addr,
  output wire rd_last,
  output wire rd_data_en,
  output wire [31:0] rd_data,

  input wire wr_req,
  input wire [8:0] wr_len,
  input wire [19:0] wr_addr,
  input wire [31:0] wr_data,
  output wire wr_last,
  output wire wr_data_en
);

    reg  [4:0]  temp_reg ;
    wire [25:0]  sram_addr;
    //******************
    //用状态机实现读与写
    //******************
    reg     [2:0]       cstate_rd;
    reg     [2:0]       nstate_rd;

    reg     [2:0]       cstate_wr;
    reg     [2:0]       nstate_wr;

    //state
    `define     IDLE_AXI    3'b000
    `define     AREAD       3'b001
    `define     READ        3'b010
    `define     AWRITE      3'b011
    `define     WRITE       3'b100
    `define     WRITE_RESP  3'b101

    //BURST
    `define     FIXED       2'b00
    `define     INCR        2'b01
    `define     WRAP        2'b10

    always @(posedge ACLK or negedge ARESETn) begin
      if(!ARESETn) begin
        cstate_rd <= `IDLE_AXI;
        cstate_wr <= `IDLE_AXI;
      end
      else  begin
        cstate_rd <= nstate_rd;
        cstate_wr <= nstate_wr;
      end
    end

    //读的状态机切换
    always @(*) begin
      case (cstate_rd)
        `IDLE_AXI: begin
          if (rd_req)
            nstate_rd = `READ;
          else
            nstate_rd = `IDLE_AXI;
          end
        `AREAD: begin
          if (ARVALID & ARREADY)
            nstate_rd = `READ;
            //注意，所有的任务结束都要跳转到 `IDLE_AXI
          else
            nstate_rd = `AREAD;
        end
        `READ: begin
          if (RLAST)
            nstate_rd = `IDLE_AXI;
          else
            nstate_rd = `READ;
        end
      endcase
    end

    //写状态机
    always @(*) begin           //state change
      case (cstate_wr)
        `IDLE_AXI: begin
          if (wr_req)
            nstate_wr = `AWRITE;
          else
            nstate_wr = `IDLE_AXI;
        end
        `AWRITE: begin
          if (AWVALID & AWREADY)
            nstate_wr = `WRITE;
          else
            nstate_wr = `AWRITE;
        end
        `WRITE: begin
          if (WLAST)
            nstate_wr = `WRITE_RESP;
          else
            nstate_wr = `WRITE;
        end
        `WRITE_RESP: begin
          if(BVALID & BREADY)
            nstate_wr = `IDLE_AXI;
          else
            nstate_wr = `WRITE_RESP;
        end
      endcase
    end

  //*****************
  //读写状态的操作
  //*****************

  //握手与响应信号
  assign ARVALID = (cstate_rd == `AREAD);
  assign ARID = 4'b0000;
  assign ARADDR = {12'b0, rd_addr};
  assign ARLEN = rd_len[7:0];
  assign ARSIZE = 3'b101; //32位
  assign ARBURST = 2'b01; //INCR
  assign ARLOCK = 1'b0;
  assign ARCACHE = 4'b0000;
  assign ARPROT = 3'b000;
  assign RREADY = (cstate_rd == `READ);

  assign AWVALID = (cstate_wr == `AWRITE);
  assign AWID = 4'b0000;
  assign AWADDR = {12'b0, wr_addr};
  assign AWLEN = wr_len[7:0];
  assign AWSIZE = 3'b101; //32位
  assign AWBURST = 2'b01; //INCR
  assign AWLOCK = 1'b0;
  assign AWCACHE = 4'b0000;
  assign AWPORT = 3'b000;

  reg [7:0] cnt_wr;
  always @(posedge ACLK or negedge ARESETn) begin
    if(!ARESETn)
      cnt_wr <= 8'b0;
    else if (WVALID & WREADY)
      cnt_wr <= cnt_wr - 1;
    else if (AWVALID & AWREADY)
      cnt_wr <= AWLEN + 1;
  end

  assign WDATA = wr_data;
  assign WSTRB = 4'b1111;
  assign WLAST = (cnt_wr == 1) & WVALID & WREADY;
  assign WVALID = (cstate_wr == `WRITE);

  assign BREADY = (cstate_wr == `WRITE_RESP);

  assign rd_last = RLAST;
  assign rd_data_en = RVALID & RREADY;
  assign rd_data = RDATA;

  assign wr_last = WLAST;
  assign wr_data_en = WVALID & WREADY;

endmodule
