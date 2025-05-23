local conf = require("undotree.config")

local time_ago = function(ptime)
	local sec = vim.fn.localtime() - ptime
	local mf = math.floor
	if sec < 60 then
		local mft = mf(sec)
		return "(" .. mft .. (mft > 1 and " secs ago)" or " sec ago)")
	elseif sec < 3600 then
		local mft = mf(sec / 60)
		return "(" .. mft .. (mft > 1 and " mins ago)" or " sec ago)")
	elseif sec < 86400 then
		local mft = mf(sec / 3600)
		return "(" .. mft .. (mft > 1 and " hours ago)" or " hour ago)")
	end
	local mft = mf(sec / 86400)
	return "(" .. mft .. (mft > 1 and " days ago)" or " day ago)")
end

--- @class Node
--- @field seq number Sequence
--- @field time number Time
--- @field save number Save
--- @field children table<Node>
--- @field indent number
--- @field parent number
local node = {}
function node:new(seq, time, save)
	return { seq = seq, time = time, save = save, children = {} }
end

local function parse_entries(input, output)
	if #input == 0 then
		return
	end
	for _, n in ipairs(input) do
		local new_node = node:new(n.seq, n.time, n.save)
		if n.alt ~= nil then
			parse_entries(n.alt, output)
		end
		table.insert(output.children, new_node)
		output = new_node
	end
end

--- @param tree Node
--- @param indent number
--- @return number
local function gen_indentions(tree, indent)
	tree.indent = indent
	local ind = indent
	for i, n in ipairs(tree.children) do
		if i ~= 1 then
			ind = ind + 1
		end
		n.parent = tree.seq
		ind = gen_indentions(n, ind)
	end
	return ind
end

--- @param tree Node
--- @return number time
local function sort_tree_by_latest(tree)
	if #tree.children == 0 then
		return tree.seq
	end

	local cur_node = tree

	while #cur_node.children <= 1 do
		if #cur_node.children == 0 then
			return cur_node.seq
		end

		cur_node = cur_node.children[1]
	end

	local max_seq = nil
	for _, sub_node in ipairs(cur_node.children) do
		sub_node.max_seq = sort_tree_by_latest(sub_node)
		if not max_seq or sub_node.max_seq > max_seq then
			max_seq = sub_node.max_seq
		end
	end

	assert(max_seq ~= -1, "Max sub node nil")

	table.sort(cur_node.children, function(a, b)
		return a.max_seq > b.max_seq
	end)

	return max_seq
end

--- @param offset integer[]
--- @param idx integer
--- @return integer off
local function offset_at_index(offset, idx)
	local off = 0
	for k = 1, idx do
		if offset[k] then
			off = off + offset[k]
		end
	end

	return off
end

--- @param graph string[]
--- @param symbol string
--- @param start_idx integer
--- @param end_idx integer
--- @param offset integer[]
local function fill_graph(graph, symbol, start_idx, end_idx, offset)
	local _start_idx = offset_at_index(offset, start_idx) + start_idx
	local _end_idx = offset_at_index(offset, end_idx) + end_idx
	if _end_idx < _start_idx then
		return
	end
	for i = _start_idx, _end_idx do
		if not graph[i] then
			graph[i] = symbol
		else
			graph[i] = string.gsub(graph[i] .. symbol, "%s+", " ")
		end
	end
end

--- @param str string
--- @return integer count
local function count_columns(str)
	local count = 0
	local i = 1
	while i <= #str do
		local char = string.sub(str, i, i)
		if char == "_" then
			if i + 1 <= #str then
				if string.sub(str, i + 1, i + 1) == "_" then
					count = count + 1
					i = i + 1
					goto continue
				end
			end
		end

		if char == "/" or char == "|" or char == "\\" or char == "*" or char == "^" then
			count = count + 1
		end

		::continue::
		i = i + 1
	end

	return count
end

--- @param graph string[]
--- @param start_idx integer
--- @param end_idx integer
--- @param offset integer[]
--- @param branch_idx integer
--- @param branches integer number of branches
local function draw_branch(graph, start_idx, end_idx, offset, branch_idx, branches)
	local _start_idx = offset_at_index(offset, start_idx) + start_idx - 1
	local _end_idx = offset_at_index(offset, end_idx) + end_idx - 1

	if _end_idx < _start_idx then
		return
	end

	for i = _start_idx, _end_idx do
		if not graph[i] then
			graph[i] = "|"
		else
			local symbol = ""

			local prev_non_spaces
			if not graph[i - 1] then
				prev_non_spaces = 0
			else
				prev_non_spaces = count_columns(graph[i - 1])
			end

			local next_non_spaces
			if not graph[i + 1] then
				next_non_spaces = 0
			else
				next_non_spaces = count_columns(graph[i + 1])
			end

			local curr_non_spaces = count_columns(graph[i])

			if prev_non_spaces < next_non_spaces + 1 then
				symbol = string.rep("__", next_non_spaces - prev_non_spaces - curr_non_spaces + 1)

				if branch_idx < branches and i == _start_idx then
					symbol = symbol .. "/_"
				else
					symbol = symbol .. "/ "
				end
			elseif prev_non_spaces > next_non_spaces + 1 then
				symbol = " \\ "
			else
				if i == _start_idx and branch_idx < branches then
					symbol = " |"
				else
					symbol = " | "
				end
			end

			graph[i] = string.gsub(graph[i] .. symbol, "%s+", " ")
		end
	end
end
--- @param graph string[]
--- @param start_idx integer
--- @param end_idx integer
--- @param offset integer[]
local function draw_line_exclusive(graph, start_idx, end_idx, offset)
	local _end_idx = offset_at_index(offset, end_idx) + end_idx - 1
	local _start_idx = (offset_at_index(offset, start_idx) + start_idx)

	if _end_idx < _start_idx then
		return
	end

	for i = _start_idx, _end_idx do
		if not graph[i] then
			graph[i] = "|"
		else
			local symbol = ""

			local prev_non_spaces
			if not graph[i - 1] then
				prev_non_spaces = 0
			else
				prev_non_spaces = count_columns(graph[i - 1])
			end

			local next_non_spaces
			if not graph[i + 1] then
				next_non_spaces = 0
			else
				next_non_spaces = count_columns(graph[i + 1])
			end

			local curr_non_spaces = count_columns(graph[i])

			if prev_non_spaces < next_non_spaces + 1 then
				symbol = string.rep("__", next_non_spaces - prev_non_spaces - curr_non_spaces + 1)
				symbol = symbol .. "/ "
			elseif prev_non_spaces > next_non_spaces + 1 then
				symbol = " \\ "
			else
				symbol = " | "
			end

			graph[i] = string.gsub(graph[i] .. symbol, "%s+", " ")
		end
	end
end

--- @param tree Node
--- @param graph string[]
--- @param parent_seq number
--- @param line2seq integer[]
--- @param other_info table<number, table>
--- @param offset integer[] Any sequence N is offset by sum of offset[i] where i is 0..N
local function _draw_new(tree, graph, parent_seq, line2seq, other_info, offset)
	local cur_node = tree
	local cur_seq = parent_seq
	while #cur_node.children == 1 do
		cur_node = cur_node.children[1]

		fill_graph(graph, " * ", cur_seq, cur_seq, offset)
		draw_line_exclusive(graph, cur_seq + 1, cur_node.seq, offset)

		cur_seq = cur_node.seq

		line2seq[cur_node.seq + offset_at_index(offset, cur_node.seq)] = cur_node.seq
		other_info[cur_node.seq] =
			{ save = cur_node.save, time = cur_node.time, lnum = cur_node.seq, parent = cur_node.parent }
	end

	if #cur_node.children == 0 then
		draw_line_exclusive(graph, cur_seq, cur_node.seq, offset)
		fill_graph(graph, " ^ ", cur_node.seq, cur_node.seq, offset)
		line2seq[cur_node.seq + offset_at_index(offset, cur_node.seq)] = cur_node.seq
		other_info[cur_node.seq] =
			{ save = cur_node.save, time = cur_node.time, lnum = cur_node.seq, parent = cur_node.parent }
		return
	end

	line2seq[cur_node.seq + offset_at_index(offset, cur_node.seq)] = cur_node.seq
	other_info[cur_node.seq] =
		{ save = cur_node.save, time = cur_node.time, lnum = cur_node.seq, parent = cur_node.parent }
	draw_line_exclusive(graph, cur_seq, cur_node.seq, offset)
	fill_graph(graph, " * ", cur_node.seq, cur_node.seq, offset)

	sort_tree_by_latest(cur_node)
	if not offset[cur_node.seq + 1] then
		offset[cur_node.seq + 1] = 1
	else
		offset[cur_node.seq + 1] = offset[cur_node.seq + 1] + 1
	end

	-- fill in existing; this is added on the right so can't change aneything that's laready there
	local branch_idx = offset_at_index(offset, cur_seq + 1) + cur_seq + 1
	if graph[branch_idx] then
		local any = false
		local insertion = ""
		for i = 1, #graph[branch_idx] do
			local char = string.sub(graph[branch_idx], i, i)
			if char ~= " " then
				if not any then
					any = true
					insertion = "|"
				else
					insertion = insertion .. " |"
				end
			end
		end

		if insertion ~= "" then
			table.insert(graph, branch_idx, insertion)
		end
	end

	-- update line2seq, if below insert, no change, if above, add 1
	local _line2seq = {}
	for k, v in pairs(line2seq) do
		if v <= cur_node.seq then
			_line2seq[k] = v
		else
			_line2seq[k + 1] = v
		end
	end

	for k, _ in pairs(line2seq) do
		line2seq[k] = nil
	end

	for k, v in pairs(_line2seq) do
		line2seq[k] = v
	end

	for i, sub_node in ipairs(cur_node.children) do
		draw_branch(graph, cur_seq + 1, sub_node.seq, offset, i, #cur_node.children)

		_draw_new(sub_node, graph, sub_node.seq, line2seq, other_info, offset)
		line2seq[sub_node.seq + offset_at_index(offset, sub_node.seq)] = sub_node.seq
		other_info[sub_node.seq] =
			{ save = sub_node.save, time = sub_node.time, lnum = sub_node.seq, parent = sub_node.parent }
	end
end

--- @param graph string[]
--- @param line2seq integer[]
local function clear_garbage(graph, line2seq)
	local n_graph = {}
	local n_line2seq = {}

	local j = 1
	while j <= #graph do
		if graph[j]:match("^%s*|%s*$") == nil then
			local n_s = graph[j]:gsub("^%s+", ""):gsub("%s+$", "")
			-- table.insert(n_graph, graph[j]:gsub("^%s+", ""):gsub("%s+$", ""))
			table.insert(n_graph, (#n_graph + 1), n_s)
			n_line2seq[#n_graph] = line2seq[j]
		end

		j = j + 1
	end

	for i, _ in pairs(graph) do
		graph[i] = nil
	end

	table.move(n_graph, 1, #n_graph, 1, graph)

	for i, _ in pairs(line2seq) do
		line2seq[i] = nil
	end

	for i, k in pairs(n_line2seq) do
		line2seq[i] = k
	end
end

-- * 6
-- | 5
-- | 4
-- | 3
-- | 2
-- * 1

--- @param tree Node
--- @param graph string[]
--- @param line2seq integer[]
--- @param other_info table<number, table>
--- @param last_seq number
local function gen_graph(tree, graph, line2seq, other_info, last_seq)
	-- local cur_seq = 1
	-- while cur_seq <= last_seq do
	-- 	draw(tree, graph, line2seq, other_info, cur_seq, 0)
	-- 	cur_seq = cur_seq + 1
	-- end

	graph[1] = "*"
	_draw_new(tree, graph, 0, line2seq, other_info, { 1 })
	graph[1] = "*"
	clear_garbage(graph, line2seq)
end

local Undotree = {}
Undotree.__index = Undotree

function Undotree:new()
	local obj = setmetatable({
		char_graph = {},
		line2seq = {},
		seq2line = {},
		seq2parent = {},
		seq_last = -1,
		seq_cur = -1,
		seq_cur_bak = -1,
	}, self)
	return obj
end

function Undotree:reset()
	self.char_graph = {}
	self.line2seq = {}
	self.seq2line = {}
	self.seq2parent = {}

	self.seq_last = -1
	self.seq_cur = -1
	self.seq_cur_bak = -1
end

function Undotree:gen_graph_tree()
	local reflash = false
	local undo_tree = vim.fn.undotree()
	self.seq_cur_bak = self.seq_cur
	self.seq_cur = undo_tree.seq_cur
	if self.seq_last ~= undo_tree.seq_last then
		reflash = true
		self:reset()
		self.seq_cur = undo_tree.seq_cur
		self.seq_last = undo_tree.seq_last

		local normal_tree = node:new(0, nil, nil)
		parse_entries(undo_tree.entries, normal_tree)
		gen_indentions(normal_tree, 0)

		local graph = {}
		local line2seq = {}
		line2seq[1] = 0
		local other_info = {}
		other_info[0] = { lnum = 1 }
		gen_graph(normal_tree, graph, line2seq, other_info, undo_tree.seq_last)

		self.seq2line[0] = #graph
		self.line2seq[#graph] = 0
		self.seq2parent[0] = nil
		graph[1] = graph[1] .. string.rep(" ", 4) .. "(Original)"

		for i = 2, #graph do
			if not graph[i] then
				local err_msg = "Index "
					.. i
					.. " absent in graph of size "
					.. #graph
					.. "\n"
					.. "seq: "
					.. (((line2seq[i] ~= nil) and line2seq[i]) or "nil")
					.. "\n"
					.. "other_info: "
					.. (((other_info[line2seq[i]] ~= nil) and other_info[line2seq[i]]) or "nil")
					.. "\n"
					.. "Nearby entries:\n"
					.. (i + 3)
					.. " : `"
					.. graph[i + 3]
					.. "`\n"
					.. (i + 2)
					.. " : `"
					.. graph[i + 2]
					.. "`\n"
					.. (i + 1)
					.. " : `"
					.. graph[i + 1]
					.. "`\n"
					.. (i - 1)
					.. " : `"
					.. graph[i - 1]
					.. "`\n"
					.. (i - 2)
					.. " : `"
					.. graph[i - 2]
					.. "`\n"
					.. (i - 3)
					.. " : `"
					.. graph[i - 3]
					.. "`\n"
				print(err_msg)

				graph[i] = "MISSING"
				goto continue
			end

			if line2seq[i] ~= nil then
				local seq = line2seq[i]
				self.seq2line[seq] = #graph - i + 1
				self.line2seq[#graph - i + 1] = seq
				self.seq2parent[seq] = other_info[seq].parent
				graph[i] = graph[i]
					.. string.rep(" ", 4)
					.. seq
					.. (other_info[seq].save and " s " or "   ")
					.. time_ago(other_info[seq].time)
			end

			::continue::
		end

		conf.reverse_table(graph, self.char_graph)
	end
	return reflash
end

return Undotree
