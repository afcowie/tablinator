username = os.getenv("USER") or "God"
password = os.getenv("PWD")  or "dontdisturb"

v = [[
<quack/>
]]

print(username .. " and " .. password)
print("«" .. v .. "»")

function SimpleProcedure (t) 
	print(t.x .. " " .. t.y);
end

SimpleProcedure {
	x=1,
	y=2
}
